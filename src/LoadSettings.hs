{-# LANGUAGE DeriveDataTypeable #-}

module LoadSettings (
    Settings (..),
    readSettingsFile, 
    writeToSettingsFile
) where

import Types
import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO 

data Settings = Settings
                     { blueName  :: String 
                     , blueImage :: String
                     , redName   :: String
                     , redImage  :: String
                     , firstPlay :: Igrac
                     } deriving (Data, Typeable)

instance Show Settings where
    show settingsObject =  "Settings{"
                        ++ "\n\tblueName:  " ++ (blueName settingsObject)  ++ ","
                        ++ "\n\tblueImage: " ++ (blueImage settingsObject) ++ ","
                        ++ "\n\tredName:   " ++ (redName settingsObject)   ++ ","
                        ++ "\n\tredImage:  " ++ (redImage settingsObject)  ++ ","
                        ++ "\n\tfirstPlay: " ++ (show $ firstPlay settingsObject)
                        ++ "\n}"

            
myOpenFile :: FilePath -> IO (String)
myOpenFile fileName = do
    pom2 <- readFile fileName
    return pom2
    
fileName :: FilePath
fileName = "src/resources/settings.json"
    
readSettingsFile :: IO (Settings)
readSettingsFile = do
    settingsJson <- myOpenFile fileName
    settingsObject <- return (decodeJSON settingsJson :: Settings)
    return settingsObject    
    
writeToSettingsFile :: Settings -> IO ()
writeToSettingsFile settingsObject = writeFile fileName (encodeJSON settingsObject)
    
