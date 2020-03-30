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
                     { igrac1_ime :: String 
                     , igrac1_boja :: String
                     , igrac1_slika :: String
                     , igrac2_ime :: String
                     , igrac2_boja :: String
                     , igrac2_slika :: String
                     , duzina_igre_u_sekundama :: String
                     , duzina_jednog_poteza_u_sekundama :: String
                     } deriving (Show, Data, Typeable)

getItem :: String -> Settings -> String
getItem item settingsObject 
    | item == "igrac1_ime" = igrac1_ime settingsObject 
    | item == "igrac1_boja" = igrac1_boja settingsObject 
    | item == "igrac1_slika" = igrac1_slika settingsObject 
    | item == "igrac2_ime" = igrac2_ime settingsObject 
    | item == "igrac2_boja" = igrac2_boja settingsObject 
    | item == "igrac2_slika" = igrac2_slika settingsObject 
    | item == "duzina_igre_u_sekundama" = duzina_igre_u_sekundama settingsObject 
    | item == "duzina_jednog_poteza_u_sekundama" = duzina_jednog_poteza_u_sekundama settingsObject 
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
    
