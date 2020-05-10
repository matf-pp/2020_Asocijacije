{-# LANGUAGE DeriveDataTypeable #-}
module GameState (
  GameState (..),
  makeGameState,
  loadGameState,
  saveGameState,
  gameState,
  getSettings,
) where

import qualified LoadSettings
import LoadAssociation
import Types

import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

data GameState = GameState { settings         :: LoadSettings.Settings
                           , playerOnMove     :: Player
                           , playerОpenedWord :: Bool
                           , player1_score    :: Int
                           , player2_score    :: Int
                           } deriving (Show)


noveSettings = LoadSettings.Settings { LoadSettings.blueName = "A" 
                                       , LoadSettings.redName  = "B"
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


makeGameState :: IO ()
makeGameState = do
    settingsObject <- LoadSettings.readSettingsFile
    makeNewAssociation
    putStrLn $ show $  (LoadSettings.firstPlay settingsObject)
    saveGameState GameState { settings = settingsObject
                            , playerOnMove = (LoadSettings.firstPlay settingsObject)
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
