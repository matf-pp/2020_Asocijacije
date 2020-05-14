module Main where

import System.Environment (getArgs)
import Data.Text (pack)

import Game (createUI)
import Types
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  args <- getArgs
  state <- newIORef (GameState {})
  createUI state $ Just $ map pack args
