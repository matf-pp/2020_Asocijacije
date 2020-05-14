{-# LANGUAGE DeriveDataTypeable #-}
module GameState (
  GameState (..),
  makeGameState,
  saveGameState,
  gameState,
  gameState'
) where

import qualified LoadSettings
import LoadAssociation
import Types
import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

makeGameState :: IORef GameState -> IO ()
makeGameState ioRef = do
    settingsObject <- LoadSettings.readSettingsFile
    assoc' <- makeNewAssociation
    writeIORef ioRef GameState { assoc = assoc'
                               , settings = settingsObject
                               , playerOnMove = (LoadSettings.firstPlay settingsObject)
                               , playerОpenedWord = False
                               , player1_score = 0
                               , player2_score = 0
                            } 

gameState :: IORef GameState -> IO GameState
gameState gs = readIORef gs

gameState' :: IORef GameState -> GameState
gameState' gs = unsafePerformIO $ readIORef gs

saveGameState ::  IORef GameState -> GameState -> IO ()
saveGameState = writeIORef
