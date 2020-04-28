{-# LANGUAGE DeriveDataTypeable #-}
module GameState (
  GameState (..),
  Igrac (..),
  makeGameState,
  loadGameState, 
  saveGameState,
  getSettings,
  getAssociation,
  getMove
) where

import qualified LoadSettings
import LoadAssociation
import Types

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO 
import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

data GameState = GameState { settings                       :: LoadSettings.Settings
                           , association                    :: Association  
                           , igracNaPotezu                  :: Igrac
                           , playerОpenedWord   :: Bool
                           , player1_score                  :: Int
                           , player2_score                  :: Int
                           } deriving (Show)


noveSettings = LoadSettings.Settings { LoadSettings.blueName = "A" 
                                       , LoadSettings.blueImage = "I"
                                       , LoadSettings.redName  = "B"
                                       , LoadSettings.redImage = "I"
                                       , LoadSettings.firstPlay = Plavi
}


noviStatus = GameState { settings = noveSettings
                                    , association = noveAsocijacije
                                    , igracNaPotezu = Plavi
                                    , playerОpenedWord = False
                                    , player1_score = 0
                                    , player2_score = 0
                                    } 


x = unsafePerformIO (newIORef noviStatus)

getSettings :: LoadSettings.Settings
getSettings = settings $ unsafePerformIO $ loadGameState

getAssociation :: Association
getAssociation = association $ unsafePerformIO $ loadGameState

getMove :: Igrac
getMove = igracNaPotezu $ unsafePerformIO $ loadGameState


makeGameState :: IO ()
makeGameState = do
    settingsObject <- LoadSettings.readSettingsFile
    associationObject <- makeNewAssociation
    let gameStateObject = GameState { settings = settingsObject
                                    , association = associationObject
                                    , igracNaPotezu = Plavi
                                    , playerОpenedWord = False
                                    , player1_score = 0
                                    , player2_score = 0
                                    }
    saveGameState gameStateObject   


loadGameState :: IO (GameState)
loadGameState = readIORef x
    
saveGameState :: GameState -> IO ()
saveGameState s = atomicModifyIORef x (\x -> (s, ()))
