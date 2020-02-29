module Main where

import Data.Default (def)
import qualified LanguageServer as LS
import State (IdeState(IdeState))


main :: IO ()
main = LS.runLanguageServer def def (\_ -> Right ()) (\_ -> Right ()) (\_ _ _ -> pure IdeState)
