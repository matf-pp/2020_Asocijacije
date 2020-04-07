{-# LANGUAGE DeriveDataTypeable #-}
module LoadSettings (
Settings (..), -- (..) eksportuje sve Data konstruktore za tip Settings
readSettingsFile, 
writeToSettingsFile,
getItem
) where

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO  

data Settings = Settings
                     { player1_name :: String 
                     , player1_color :: String
                     , player1_image :: String
                     , player2_name :: String
                     , player2_color :: String
                     , player2_image :: String
                     , game_duration_in_seconds :: String
                     , waiting_time_for_one_play_in_seconds :: String
                     } deriving (Show, Data, Typeable)

getItem :: String -> Settings -> String
getItem item settingsObject 
    | item == "player1_name" = player1_name settingsObject 
    | item == "player1_color" = player1_color settingsObject 
    | item == "player1_image" = player1_image settingsObject 
    | item == "player2_name" = player2_name settingsObject 
    | item == "player2_color" = player2_color settingsObject 
    | item == "player2_image" = player2_image settingsObject 
    | item == "game_duration_in_seconds" = game_duration_in_seconds settingsObject 
    | item == "waiting_time_for_one_play_in_seconds" = waiting_time_for_one_play_in_seconds settingsObject 
    | otherwise = ""
                   
myOpenFile :: FilePath -> IO (String)
myOpenFile fileName = do
    pom2 <- readFile fileName
    return pom2
    
makeFileName :: FilePath
makeFileName = "src/resources/settings.json"
    
readSettingsFile :: IO (Settings)
readSettingsFile = do
    let fileName = makeFileName
    settingsJson <- myOpenFile fileName
    settingsObject <- return (decodeJSON settingsJson :: Settings)
    return settingsObject    
    
writeToSettingsFile :: Settings -> IO ()
writeToSettingsFile settingsObject = do
    let fileName = makeFileName
    let sadrzaj = (encodeJSON settingsObject)
    writeFile fileName sadrzaj
    
