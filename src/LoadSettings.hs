{-# LANGUAGE DeriveDataTypeable #-}

module LoadSettings (
    Settings (..),
    readSettingsFile, 
    writeToSettingsFile
) where


import Types
import Text.JSON.Generic
import System.IO 


fileName :: FilePath
fileName = "settings.json"
    

readSettingsFile :: IO (Settings)
readSettingsFile = do
    settingsJson <- readFile fileName
    return (decodeJSON settingsJson :: Settings)    


writeToSettingsFile :: Settings -> IO ()
writeToSettingsFile settingsObject = writeFile fileName (encodeJSON settingsObject)
