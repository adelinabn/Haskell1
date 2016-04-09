module Main where

import REPL
import System.Console.Haskeline

-- | Main function of the program, calling the repl loop.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = repl initState
