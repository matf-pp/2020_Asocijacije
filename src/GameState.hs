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

data GameState = GameState { settings                :: LoadSettings.Settings
                           , association             :: LoadAssociation.Association  
                           , reaming_time_in_seconds :: String  
                           --, on_move                 :: Int
                           } deriving (Data, Typeable)

getSettings :: GameState -> LoadSettings.Settings
getSettings gameStateObject = settings gameStateObject

getAssociation :: GameState -> LoadAssociation.Association 
getAssociation gameStateObject = association gameStateObject

instance Show GameState where
    show gameStateObject = "GameState{"
                        ++ "\n" ++ (show $ settings gameStateObject) ++ ","
                        ++ "\n" ++ (show $ association gameStateObject) ++ ","
                        ++ "\nreaming_time_in_seconds: " ++ (reaming_time_in_seconds gameStateObject)
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
                                    , reaming_time_in_seconds = LoadSettings.getItem "game_duration_in_seconds" settingsObject 
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
