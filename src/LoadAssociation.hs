{-# LANGUAGE DeriveDataTypeable #-}
module LoadAssociation 
(
    Association (..), 
    makeNewAssociation, 
    uzmiPolje,
    uzmiKolonu,
    getWord,
    getIsOpened,
    noveAsocijacije,
    openAssociation
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
import qualified Data.Map as Map

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


uzmiPolje :: Field -> PairWordIsOpened 
uzmiPolje (A,F1) = a1_private association
uzmiPolje (A,F2) = a2_private association
uzmiPolje (A,F3) = a3_private association
uzmiPolje (A,F4) = a4_private association
uzmiPolje (B,F1) = b1_private association
uzmiPolje (B,F2) = b2_private association
uzmiPolje (B,F3) = b3_private association
uzmiPolje (B,F4) = b4_private association
uzmiPolje (C,F1) = c1_private association
uzmiPolje (C,F2) = c2_private association
uzmiPolje (C,F3) = c3_private association
uzmiPolje (C,F4) = c4_private association
uzmiPolje (D,F1) = d1_private association
uzmiPolje (D,F2) = d2_private association
uzmiPolje (D,F3) = d3_private association
uzmiPolje (D,F4) = d4_private association


uzmiKolonu :: Maybe Column -> PairWordIsOpened
uzmiKolonu (Just A) = a_private association
uzmiKolonu (Just B) = b_private association
uzmiKolonu (Just C) = c_private association
uzmiKolonu (Just D) = d_private association
uzmiKolonu Nothing  = final_private association


openAssociation :: Field -> IO ()
openAssociation (A,F1) = do setAssociation association{ a1_private = (fst (a1_private association), True)}
openAssociation (A,F2) = do setAssociation association{ a2_private = (fst (a2_private association), True)}
openAssociation (A,F3) = do setAssociation association{ a3_private = (fst (a3_private association), True)}
openAssociation (A,F4) = do setAssociation association{ a4_private = (fst (a4_private association), True)}
openAssociation (B,F1) = do setAssociation association{ b1_private = (fst (b1_private association), True)}
openAssociation (B,F2) = do setAssociation association{ b2_private = (fst (b2_private association), True)}
openAssociation (B,F3) = do setAssociation association{ b3_private = (fst (b3_private association), True)}
openAssociation (B,F4) = do setAssociation association{ b4_private = (fst (b4_private association), True)}
openAssociation (C,F1) = do setAssociation association{ c1_private = (fst (c1_private association), True)}
openAssociation (C,F2) = do setAssociation association{ c2_private = (fst (c2_private association), True)}
openAssociation (C,F3) = do setAssociation association{ c3_private = (fst (c3_private association), True)}
openAssociation (C,F4) = do setAssociation association{ c4_private = (fst (c4_private association), True)}
openAssociation (D,F1) = do setAssociation association{ d1_private = (fst (d1_private association), True)}
openAssociation (D,F2) = do setAssociation association{ d2_private = (fst (d2_private association), True)}
openAssociation (D,F3) = do setAssociation association{ d3_private = (fst (d3_private association), True)}
openAssociation (D,F4) = do setAssociation association{ d4_private = (fst (d4_private association), True)}


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
    
makeNewAssociation :: IO ()
makeNewAssociation = do
    associationOnlyJson <- myOpenFile makeFileName
    associationOnlyObject <- return (decodeJSON associationOnlyJson :: AssociationOnly)
    setAssociation Association { a1_private = (a1 associationOnlyObject, False)
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

