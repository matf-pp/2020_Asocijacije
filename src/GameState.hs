{-# LANGUAGE DeriveDataTypeable #-}
module GameState (
  GameState (..),
  Igrac (..),
  makeGameState,
  loadGameState,
  saveGameState,
  gameState,
  getSettings,
  getAssociation,
  getMove
) where

import qualified LoadSettings
import LoadAssociation
import Types

import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

data GameState = GameState { settings         :: LoadSettings.Settings
                           , playerOnMove     :: Igrac
                           , playerОpenedWord :: Bool
                           , player1_score    :: Int
                           , player2_score    :: Int
                           } deriving (Show)


noveSettings = LoadSettings.Settings { LoadSettings.blueName = "A" 
                                       , LoadSettings.blueImage = "I"
                                       , LoadSettings.redName  = "B"
                                       , LoadSettings.redImage = "I"
                                       , LoadSettings.firstPlay = Plavi
}


noviStatus = GameState { settings = noveSettings
                                    , playerOnMove = Plavi
                                    , playerОpenedWord = False
                                    , player1_score = 0
                                    , player2_score = 0
                                    } 


xGameState = unsafePerformIO (newIORef (GameState {}))


getSettings :: LoadSettings.Settings
getSettings = settings $ unsafePerformIO $ loadGameState


getAssociation :: Association
getAssociation = association


getMove :: Igrac
getMove = playerOnMove $ unsafePerformIO $ loadGameState


makeGameState :: IO ()
makeGameState = do
    settingsObject <- LoadSettings.readSettingsFile
    makeNewAssociation
    saveGameState GameState { settings = settingsObject
                            , playerOnMove = Plavi
                            , playerОpenedWord = False
                            , player1_score = 0
                            , player2_score = 0
                            } 

gameState :: GameState
gameState =  unsafePerformIO $ readIORef xGameState


loadGameState :: IO (GameState)
loadGameState = readIORef xGameState
    
saveGameState :: GameState -> IO ()
saveGameState s = atomicModifyIORef xGameState (\x -> (s, ()))
