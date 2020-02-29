{-# language
  ExistentialQuantification, LambdaCase, RecordWildCards,
  ScopedTypeVariables, NamedFieldPuns, OverloadedStrings #-}

module LanguageServer
    ( runLanguageServer
    ) where

import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
-- import qualified Development.IDE.GHC.Util as Ghcide
import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core as LSP
import Control.Concurrent.Chan
import Control.Concurrent.Extra
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Data.Default (def)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.IO.Handle (hDuplicate)
import System.IO
import Control.Monad.Extra

import           Server (WithMessage(..), PartialHandlers(..))
import           State (IdeState)
import           Notifications (setHandlersNotifications)
-- import Development.IDE.Core.IdeConfiguration
-- import Development.IDE.Core.Shake
-- import Development.IDE.LSP.HoverDefinition
-- import Development.IDE.LSP.Notifications
-- import Development.IDE.LSP.Outline
-- import Development.IDE.Types.Logger
-- import Development.IDE.Core.FileStore
import Language.Haskell.LSP.Core (LspFuncs(..))
import Language.Haskell.LSP.Messages

runLanguageServer
    :: forall config. (Show config)
    => LSP.Options
    -> PartialHandlers config
    -> (InitializeRequest -> Either T.Text config)
    -> (DidChangeConfigurationNotification -> Either T.Text config)
    -> (IO LspId -> (FromServerMessage -> IO ()) -> ClientCapabilities -> IO IdeState)
    -> IO ()
runLanguageServer options userHandlers onInitialConfig onConfigChange getIdeState = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan (Message config) <- newChan

    -- These barriers are signaled when the threads reading from these chans exit.
    -- This should not happen but if it does, we will make sure that the whole server
    -- dies and can be restarted instead of losing threads silently.
    clientMsgBarrier <- newBarrier

    -- The set of requests ids that we have received but not finished processing
    pendingRequests <- newTVarIO Set.empty
    -- The set of requests that have been cancelled and are also in pendingRequests
    cancelledRequests <- newTVarIO Set.empty

    let withResponse wrap f = Just $ \r@RequestMessage{_id} -> do
            atomically $ modifyTVar pendingRequests (Set.insert _id)
            writeChan clientMsgChan $ Response r wrap f
    let withNotification old f = Just $ \r -> writeChan clientMsgChan $ Notification r (\lsp ide x -> f lsp ide x >> whenJust old ($ r))
    let withResponseAndRequest wrap wrapNewReq f = Just $ \r@RequestMessage{_id} -> do
            atomically $ modifyTVar pendingRequests (Set.insert _id)
            writeChan clientMsgChan $ ResponseAndRequest r wrap wrapNewReq f
    let withInitialize f = Just $ \r -> writeChan clientMsgChan $ InitialParams r (\lsp ide x -> f lsp ide x)
    let cancelRequest reqId = atomically $ do
            queued <- readTVar pendingRequests
            -- We want to avoid that the list of cancelled requests
            -- keeps growing if we receive cancellations for requests
            -- that do not exist or have already been processed.
            when (reqId `elem` queued) $
                modifyTVar cancelledRequests (Set.insert reqId)
    let clearReqId reqId = atomically $ do
            modifyTVar pendingRequests (Set.delete reqId)
            modifyTVar cancelledRequests (Set.delete reqId)
        -- We implement request cancellation by racing waitForCancel against
        -- the actual request handler.
    let waitForCancel reqId = atomically $ do
            cancelled <- readTVar cancelledRequests
            unless (reqId `Set.member` cancelled) retry
    let PartialHandlers parts =
            initializeRequestHandler <>
            setHandlersIgnore <> -- least important
            userHandlers <>
            setHandlersNotifications <> -- absolutely critical, join them with user notifications
            cancelHandler cancelRequest
            -- Cancel requests are special since they need to be handled
            -- out of order to be useful. Existing handlers are run afterwards.
    handlers <- parts WithMessage{withResponse, withNotification, withResponseAndRequest, withInitialize} def

    let initializeCallbacks = LSP.InitializeCallbacks
            { LSP.onInitialConfiguration = onInitialConfig
            , LSP.onConfigurationChange = onConfigChange
            , LSP.onStartup = handleInit (signalBarrier clientMsgBarrier ()) clearReqId waitForCancel clientMsgChan
            }

    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runWithHandles
            stdin
            stdout
            initializeCallbacks
            handlers
            (modifyOptions options)
            Nothing
        , void $ waitBarrier clientMsgBarrier
        ]
    where
        handleInit :: IO () -> (LspId -> IO ()) -> (LspId -> IO ()) -> Chan (Message config) -> LSP.LspFuncs config -> IO (Maybe err)
        handleInit exitClientMsg clearReqId waitForCancel clientMsgChan lspFuncs@LSP.LspFuncs{..} = do

            ide <- getIdeState getNextReqId sendFunc clientCapabilities

            _ <- flip forkFinally (const exitClientMsg) $ forever $ do
                msg <- readChan clientMsgChan
                case msg of
                    Notification x@NotificationMessage{_params} act ->
                        act lspFuncs ide _params
                            -- logError (ideLogger ide) $ T.pack $
                            --     "Unexpected exception on notification, please report!\n" ++
                            --     "Message: " ++ show x ++ "\n" ++
                            --     "Exception: " ++ show e
                    Response x@RequestMessage{_id, _params} wrap act ->
                        checkCancelled ide clearReqId waitForCancel lspFuncs wrap act x _id _params $
                            \case
                              Left e  -> sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) Nothing (Just e)
                              Right r -> sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) (Just r) Nothing
                    ResponseAndRequest x@RequestMessage{_id, _params} wrap wrapNewReq act ->
                        checkCancelled ide clearReqId waitForCancel lspFuncs wrap act x _id _params $
                            \(res, newReq) -> do
                            sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) (Just res) Nothing
                            case newReq of
                                Nothing -> return ()
                                Just (rm, newReqParams) -> do
                                    reqId <- getNextReqId
                                    sendFunc $ wrapNewReq $ RequestMessage "2.0" reqId rm newReqParams
                    InitialParams x@RequestMessage{_id, _params} act -> do
                        act lspFuncs ide _params
                            -- logError (ideLogger ide) $ T.pack $
                            --     "Unexpected exception on InitializeRequest handler, please report!\n" ++
                            --     "Message: " ++ show x ++ "\n" ++
                            --     "Exception: " ++ show e
            pure Nothing

        checkCancelled ide clearReqId waitForCancel lspFuncs@LSP.LspFuncs{..} wrap act msg _id _params k =
            flip finally (clearReqId _id) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (waitForCancel _id) $ act lspFuncs ide _params
                    case cancelOrRes of
                        Left () -> do
                            -- logDebug (ideLogger ide) $ T.pack $
                            --     "Cancelled request " <> show _id
                            sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) Nothing $
                                Just $ ResponseError RequestCancelled "" Nothing
                        Right res -> k res
                ) $ \(e :: SomeException) -> do
                    -- logError (ideLogger ide) $ T.pack $
                    --     "Unexpected exception on request, please report!\n" ++
                    --     "Message: " ++ show msg ++ "\n" ++
                    --     "Exception: " ++ show e
                    sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) Nothing $
                        Just $ ResponseError InternalError (T.pack $ show e) Nothing

initializeRequestHandler :: PartialHandlers config
initializeRequestHandler = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.initializeRequestHandler = withInitialize initHandler
    }

initHandler
    :: LSP.LspFuncs c
    -> IdeState
    -> InitializeParams
    -> IO ()
initHandler _ ide params = pure () -- registerIdeConfiguration (shakeExtras ide) (parseConfiguration params)

-- | Things that get sent to us, but we don't deal with.
--   Set them to avoid a warning in VS Code output.
setHandlersIgnore :: PartialHandlers config
setHandlersIgnore = PartialHandlers $ \_ x -> return x
    {LSP.initializedHandler = none
    ,LSP.responseHandler = none
    }
    where none = Just $ const $ return ()

cancelHandler :: (LspId -> IO ()) -> PartialHandlers config
cancelHandler cancelRequest = PartialHandlers $ \_ x -> return x
    {LSP.cancelNotificationHandler = Just $ \msg@NotificationMessage {_params = CancelParams {_id}} -> do
            cancelRequest _id
            whenJust (LSP.cancelNotificationHandler x) ($ msg)
    }


-- | A message that we need to deal with - the pieces are split up with existentials to gain additional type safety
--   and defer precise processing until later (allows us to keep at a higher level of abstraction slightly longer)
data Message c
    = forall m req resp . (Show m, Show req) => Response (RequestMessage m req resp) (ResponseMessage resp -> FromServerMessage) (LSP.LspFuncs c -> IdeState -> req -> IO (Either ResponseError resp))
    -- | Used for cases in which we need to send not only a response,
    --   but also an additional request to the client.
    --   For example, 'executeCommand' may generate an 'applyWorkspaceEdit' request.
    | forall m rm req resp newReqParams newReqBody . (Show m, Show rm, Show req) => ResponseAndRequest (RequestMessage m req resp) (ResponseMessage resp -> FromServerMessage) (RequestMessage rm newReqParams newReqBody -> FromServerMessage) (LSP.LspFuncs c -> IdeState -> req -> IO (resp, Maybe (rm, newReqParams)))
    | forall m req . (Show m, Show req) => Notification (NotificationMessage m req) (LSP.LspFuncs c -> IdeState -> req -> IO ())
    -- | Used for the InitializeRequest only, where the response is generated by the LSP core handler.
    | InitialParams InitializeRequest (LSP.LspFuncs c -> IdeState -> InitializeParams -> IO ())

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.textDocumentSync   = Just $ tweakTDS origTDS
                   , LSP.executeCommandCommands = Just ["typesignature.add"]
                   , LSP.completionTriggerCharacters = Just ['.']
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing
