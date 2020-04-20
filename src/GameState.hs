{-# LANGUAGE DeriveDataTypeable #-}
module GameState (
GameState (..),
makeGameState,
loadGameState, 
saveGameState,
getSettings,
getAssociation
) where

import qualified LoadSettings
import LoadAssociation
import Types

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO 
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data GameState = GameState { settings                       :: LoadSettings.Settings
                           , association                    :: Association  
                           , reaming_time_in_seconds        :: Int  
                           , on_move                        :: Int
                           , did_on_move_player_open_word   :: Bool
                           , player1_score                  :: Int
                           , player2_score                  :: Int
                           } deriving (Show)


noveSettings = LoadSettings.Settings { LoadSettings.player1_name = "A" 
                                       , LoadSettings.player1_color = "C"
                                       , LoadSettings.player1_image = "I"
                                       , LoadSettings.player2_name  = "B"
                                       , LoadSettings.player2_color = "C"
                                       , LoadSettings.player2_image = "I"
                                       , LoadSettings.game_duration_in_seconds = "10"
                                       , LoadSettings.waiting_time_for_one_play_in_seconds = "10"
                                       , LoadSettings.first_play = "A"
}


noviStatus = GameState { settings = noveSettings
                                    , association = noveAsocijacije
                                    , reaming_time_in_seconds = read $ LoadSettings.getItem "game_duration_in_seconds" noveSettings :: Int
                                    , on_move = read $ LoadSettings.getItem "first_play" noveSettings :: Int
                                    , did_on_move_player_open_word = False
                                    , player1_score = 0
                                    , player2_score = 0
                                    } 


x = unsafePerformIO (newIORef noviStatus)

getSettings :: GameState -> LoadSettings.Settings
getSettings gameStateObject = settings gameStateObject

getAssociation :: GameState -> Association 
getAssociation gameStateObject = association gameStateObject


makeGameState :: IO ()
makeGameState = do
    settingsObject <- LoadSettings.readSettingsFile
    associationObject <- makeNewAssociation
    let gameStateObject = GameState { settings = settingsObject
                                    , association = associationObject
                                    , reaming_time_in_seconds = read $ LoadSettings.getItem "game_duration_in_seconds" settingsObject :: Int
                                    , on_move = read $ LoadSettings.getItem "first_play" settingsObject :: Int
                                    , did_on_move_player_open_word = False
                                    , player1_score = 0
                                    , player2_score = 0
                                    }
    saveGameState gameStateObject   


loadGameState :: IO (GameState)
loadGameState = readIORef x
    
saveGameState :: GameState -> IO ()
saveGameState s = atomicModifyIORef x (\x -> (s, ()))
