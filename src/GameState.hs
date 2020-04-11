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
import qualified LoadAssociation

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO  

data GameState = GameState { settings                       :: LoadSettings.Settings
                           , association                    :: LoadAssociation.Association  
                           , reaming_time_in_seconds        :: Int  
                           , on_move                        :: Int
                           , did_on_move_player_open_word   :: Bool
                           , player1_score                  :: Int
                           , player2_score                  :: Int
                           } deriving (Data, Typeable)

getSettings :: GameState -> LoadSettings.Settings
getSettings gameStateObject = settings gameStateObject

getAssociation :: GameState -> LoadAssociation.Association 
getAssociation gameStateObject = association gameStateObject

instance Show GameState where
    show gameStateObject = "GameState{"
                        ++ "\n" ++ (show $ settings gameStateObject) ++ ","
                        ++ "\n" ++ (show $ association gameStateObject) ++ ","
                        ++ "\nreaming_time_in_seconds: " ++ (show $ reaming_time_in_seconds gameStateObject)
                        ++ "\non_move: " ++ (show $ on_move gameStateObject)
                        ++ "\ndid_on_move_player_open_word: " ++ (show $ did_on_move_player_open_word gameStateObject)
                        ++ "\nplayer1_score: " ++ (show $ player1_score gameStateObject)
                        ++ "\nplayer2_score: " ++ (show $ player2_score gameStateObject)
                        ++ "\n}"
                   
myOpenFile :: FilePath -> IO (String)
myOpenFile fileName = do
    pom2 <- readFile fileName
    return pom2
    
makeFileName :: FilePath
makeFileName = ".cabal-sandbox/bin/gameState.json"
    
makeGameState :: IO ()
makeGameState = do
    settingsObject <- LoadSettings.readSettingsFile
    associationObject <- LoadAssociation.makeNewAssociation
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
loadGameState = do
    let fileName = makeFileName
    gameStateJson <- myOpenFile fileName
    gameStateObject <- return (decodeJSON gameStateJson :: GameState)
    return gameStateObject    
    
saveGameState :: GameState -> IO ()
saveGameState gameStateObject = do
    let fileName = makeFileName
    let sadrzaj = (encodeJSON gameStateObject)
    writeFile fileName sadrzaj 
