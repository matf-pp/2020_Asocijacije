module Main where

import System.Environment (getArgs)
import Data.Text (pack)

import UI (createUI)

main :: IO ()
main = do
  args <- getArgs
  createUI $ Just $ map pack args
--   print "Hello"
