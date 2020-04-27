{-# LANGUAGE DeriveDataTypeable #-}
module LoadAssociation 
(
    Association (..), 
    makeNewAssociation, 
    uzmiPolje,
    uzmiKolonu,
    setItem,
    postaviKolonu,
    getWord,
    getIsOpened,
    noveAsocijacije
) where

import Types

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO  
import System.IO.Error
import Control.Exception
import System.Directory
import System.Random

getWord :: PairWordIsOpened -> String
getWord pairWordIsOpenedObject = fst pairWordIsOpenedObject

getIsOpened :: PairWordIsOpened -> Bool
getIsOpened pairWordIsOpenedObject = snd pairWordIsOpenedObject

noveAsocijacije =  Association { a1_private = ("A1", False) 
                               , a2_private = ("A2", False) 
                               , a3_private = ("A3", False) 
                               , a4_private = ("A4", False) 
                               , a_private  = ("A", False)
                               , b1_private = ("B1", False) 
                               , b2_private = ("B2", False) 
                               , b3_private = ("B3", False) 
                               , b4_private = ("B4", False) 
                               , b_private  = ("B", False)
                               , c1_private = ("C1", False) 
                               , c2_private = ("C2", False) 
                               , c3_private = ("C3", False) 
                               , c4_private = ("C4", False) 
                               , c_private  = ("C", False)
                               , d1_private = ("D1", False) 
                               , d2_private = ("D2", False) 
                               , d3_private = ("D3", False) 
                               , d4_private = ("D4", False) 
                               , d_private  = ("D", False)
                               , final_private = ("F", False) }


data AssociationOnly = AssociationOnly
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
                     } deriving (Data, Typeable, Show)


uzmiPolje :: Polje -> Association -> PairWordIsOpened
uzmiPolje polje associationObject 
    | polje == (A,F1) = a1_private associationObject
    | polje == (A,F2) = a2_private associationObject
    | polje == (A,F3) = a3_private associationObject
    | polje == (A,F4) = a4_private associationObject
    | polje == (B,F1) = b1_private associationObject
    | polje == (B,F2) = b2_private associationObject
    | polje == (B,F3) = b3_private associationObject
    | polje == (B,F4) = b4_private associationObject
    | polje == (C,F1) = c1_private associationObject
    | polje == (C,F2) = c2_private associationObject
    | polje == (C,F3) = c3_private associationObject
    | polje == (C,F4) = c4_private associationObject
    | polje == (D,F1) = d1_private associationObject
    | polje == (D,F2) = d2_private associationObject
    | polje == (D,F3) = d3_private associationObject
    | polje == (D,F4) = d4_private associationObject


uzmiKolonu :: Maybe Kolona -> Association -> PairWordIsOpened
uzmiKolonu (Just kolona) associationObject
    | kolona == A = a_private associationObject
    | kolona == B = c_private associationObject
    | kolona == C = c_private associationObject
    | kolona == D = d_private associationObject
uzmiKolonu Nothing associationObject = final_private associationObject


setItem :: Polje -> String -> Bool -> Association -> Association
setItem polje word_ isOpened_ associationObject
    | polje == (A,F1) = associationObject{a1_private= (word_, isOpened_)}
    | polje == (A,F2) = associationObject{a2_private= (word_, isOpened_)}
    | polje == (A,F3) = associationObject{a3_private= (word_, isOpened_)}
    | polje == (A,F4) = associationObject{a4_private= (word_, isOpened_)}
    | polje == (B,F1) = associationObject{b1_private= (word_, isOpened_)}
    | polje == (B,F2) = associationObject{b2_private= (word_, isOpened_)}
    | polje == (B,F3) = associationObject{b3_private= (word_, isOpened_)}
    | polje == (B,F4) = associationObject{b4_private= (word_, isOpened_)}
    | polje == (C,F1) = associationObject{c1_private= (word_, isOpened_)}
    | polje == (C,F2) = associationObject{c2_private= (word_, isOpened_)}
    | polje == (C,F3) = associationObject{c3_private= (word_, isOpened_)}
    | polje == (C,F4) = associationObject{c4_private= (word_, isOpened_)}
    | polje == (D,F1) = associationObject{d1_private= (word_, isOpened_)}
    | polje == (D,F2) = associationObject{d2_private= (word_, isOpened_)}
    | polje == (D,F3) = associationObject{d3_private= (word_, isOpened_)}
    | polje == (D,F4) = associationObject{d4_private= (word_, isOpened_)}



postaviKolonu :: Maybe Kolona -> String -> Bool -> Association -> Association
postaviKolonu (Just kolona) word_ isOpened_ associationObject   
    | kolona == A = associationObject{a_private = (word_, isOpened_)}
    | kolona == B = associationObject{b_private = (word_, isOpened_)}
    | kolona == C = associationObject{c_private = (word_, isOpened_)}
    | kolona == D = associationObject{d_private = (word_, isOpened_)}
postaviKolonu (Nothing) word_ isOpened_ associationObject  = associationObject{final_private=(word_, isOpened_)}


myOpenFile :: IO FilePath -> IO (String)
myOpenFile fileName = do
    pom1 <- fileName
    pom2 <- readFile pom1
    return pom2
   
makeFileName :: IO FilePath
makeFileName = do
    randomBrojFajla <- randomFajl $ directorySize $ directoryWithAssociationsPath
    return $ directoryWithAssociationsPath ++ "/asocijacija" ++ (show randomBrojFajla) ++ ".json"
    
randomFajl :: IO Int -> IO Int
randomFajl nekiInt = do
    pom <- nekiInt
    num <- getStdRandom (randomR (1,pom))  
    return num
    
directoryWithAssociationsPath :: FilePath
directoryWithAssociationsPath = "src/folderSaAsocijacijama"

directorySize :: FilePath -> IO Int
directorySize dir = fmap length . listDirectory $ directoryWithAssociationsPath
    
makeNewAssociation :: IO (Association)
makeNewAssociation = do
    let fileName = makeFileName
    associationOnlyJson <- myOpenFile fileName
    associationOnlyObject <- return (decodeJSON associationOnlyJson :: AssociationOnly)
    return Association  { a1_private = (a1 associationOnlyObject, False)
                        , a2_private = (a2 associationOnlyObject, False)
                        , a3_private = (a3 associationOnlyObject, False)
                        , a4_private = (a4 associationOnlyObject, False)
                        , a_private  = (a associationOnlyObject,  False)
                        , b1_private = (b1 associationOnlyObject, False)
                        , b2_private = (b2 associationOnlyObject, False)
                        , b3_private = (b3 associationOnlyObject, False)
                        , b4_private = (b4 associationOnlyObject, False)
                        , b_private  = (b associationOnlyObject,  False)
                        , c1_private = (c1 associationOnlyObject, False)
                        , c2_private = (c2 associationOnlyObject, False)
                        , c3_private = (c3 associationOnlyObject, False)
                        , c4_private = (c4 associationOnlyObject, False)
                        , c_private  = (c associationOnlyObject,  False)
                        , d1_private = (d1 associationOnlyObject, False)
                        , d2_private = (d2 associationOnlyObject, False)
                        , d3_private = (d3 associationOnlyObject, False)
                        , d4_private = (d4 associationOnlyObject, False)
                        , d_private  = (d associationOnlyObject,  False)
                        , final_private = (final associationOnlyObject, False)
                        }    

