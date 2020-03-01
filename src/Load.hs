{-# LANGUAGE DeriveDataTypeable #-}
module Load (WordAssociationTableContent, makeNewWordAssociation, getItem) where

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO  
import System.IO.Error
import Control.Exception
import System.Directory
import System.Random

data WordAssociationTableContent = WordAssociationTableContent
                     { a1 :: String 
                     , a2 :: String
                     , a3 :: String
                     , a4 :: String
                     , a :: String
                     , b1 :: String
                     , b2 :: String
                     , b3 :: String
                     , b4 :: String
                     , b :: String
                     , c1 :: String
                     , c2 :: String
                     , c3 :: String
                     , c4 :: String
                     , c :: String
                     , d1 :: String
                     , d2 :: String
                     , d3 :: String
                     , d4 :: String
                     , d :: String
                     , final :: String
                     } deriving (Show, Data, Typeable)

getItem :: String -> WordAssociationTableContent -> String
getItem item wordAssociationTableContentObject 
    | item == "a1" = a1 wordAssociationTableContentObject
    | item == "a2" = a2 wordAssociationTableContentObject
    | item == "a3" = a3 wordAssociationTableContentObject
    | item == "a4" = a4 wordAssociationTableContentObject
    | item == "a" = a wordAssociationTableContentObject
    | item == "b1" = b1 wordAssociationTableContentObject
    | item == "b2" = b2 wordAssociationTableContentObject
    | item == "b3" = b3 wordAssociationTableContentObject
    | item == "b4" = b4 wordAssociationTableContentObject
    | item == "c" = c wordAssociationTableContentObject
    | item == "c1" = c1 wordAssociationTableContentObject
    | item == "c2" = c2 wordAssociationTableContentObject
    | item == "c3" = c3 wordAssociationTableContentObject
    | item == "c4" = c4 wordAssociationTableContentObject
    | item == "c" = c wordAssociationTableContentObject
    | item == "d1" = d1 wordAssociationTableContentObject
    | item == "d2" = d2 wordAssociationTableContentObject
    | item == "d3" = d3 wordAssociationTableContentObject
    | item == "d4" = d4 wordAssociationTableContentObject
    | item == "d" = d wordAssociationTableContentObject
    | item == "final" = final wordAssociationTableContentObject
    | otherwise = "" --die "Bad function call. Function name: getItem, from source file load.hs."
                   
myOpenFile :: IO FilePath -> IO (String)
myOpenFile fileName = do
    pom1 <- fileName
    pom2 <- readFile pom1
    return pom2
  
handlerForTryToOpenFile :: IOError -> IO ()  
handlerForTryToOpenFile e = die "\nBad function call. Function name: handlerForTryToOpenFile, from source file load.hs. Error is probably in function makeFileName, because fileName was generated here." -- ++ e
    
makeFileName :: IO FilePath
makeFileName = do
    randomBrojFajla <- randomFajl $ directorySize $ directoryWithAssociationsPath
    return $ "./folderSaAsocijacijama/asocijacija" ++ (show randomBrojFajla) ++ ".json"
    
randomFajl :: IO Int -> IO Int
randomFajl nekiInt = do
    pom <- nekiInt
    num <- getStdRandom (randomR (1,pom))  
    return num
    
directoryWithAssociationsPath :: FilePath
directoryWithAssociationsPath = "./folderSaAsocijacijama"

directorySize :: FilePath -> IO Int
directorySize dir = fmap length . listDirectory $ "./folderSaAsocijacijama/"
    
makeNewWordAssociation :: IO (WordAssociationTableContent)
makeNewWordAssociation = do
    let fileName = makeFileName
--     PRVA VERZIJA BEZ PROVERA I IZUZETAKA
    wordAssociationTableContentJson <- myOpenFile fileName
    wordAssociationTableContentObject <- return (decodeJSON wordAssociationTableContentJson :: WordAssociationTableContent)
    return wordAssociationTableContentObject    
--     DRUGA VERZIJA SA PROVEROM I IZUZECIMA
--     wordAssociationTableContentJson <- try (myOpenFile fileName) :: IO (Either IOException String)
--     case wordAssociationTableContentJson of 
--          Left e -> do
--              handlerForTryToOpenFile e
--          Right wordAssociationTableContentJson -> do
--             wordAssociationTableContentObject <- return (decodeJSON wordAssociationTableContentJson :: WordAssociationTableContent)
--             return $ wordAssociationTableContentObject
