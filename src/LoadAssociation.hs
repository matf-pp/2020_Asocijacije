{-# LANGUAGE DeriveDataTypeable #-}
module LoadAssociation 
(
    Association (..), 
    makeNewAssociation, 
    getItem, 
    setItem,
    getWord,
    getIsOpened
) where

import Text.JSON.Generic
import System.Exit
import System.Environment  
import System.IO  
import System.IO.Error
import Control.Exception
import System.Directory
import System.Random

data PairWordIsOpened = PairWordIsOpened { word :: String
                                         , isOpened :: Bool
                                         } deriving (Data, Typeable)

getWord :: PairWordIsOpened -> String
getWord pairWordIsOpenedObject = word pairWordIsOpenedObject

getIsOpened :: PairWordIsOpened -> Bool
getIsOpened pairWordIsOpenedObject = isOpened pairWordIsOpenedObject

instance Show PairWordIsOpened where
    show pairWordIsOpenedObject = "(word: " ++ (word pairWordIsOpenedObject) ++ ", isOpened: " ++ (show $ isOpened pairWordIsOpenedObject) ++ ")"


data Association = Association   { a1_private :: PairWordIsOpened 
                                 , a2_private :: PairWordIsOpened
                                 , a3_private :: PairWordIsOpened
                                 , a4_private :: PairWordIsOpened
                                 , a_private :: PairWordIsOpened
                                 , b1_private :: PairWordIsOpened
                                 , b2_private :: PairWordIsOpened
                                 , b3_private :: PairWordIsOpened
                                 , b4_private :: PairWordIsOpened
                                 , b_private :: PairWordIsOpened
                                 , c1_private :: PairWordIsOpened
                                 , c2_private :: PairWordIsOpened
                                 , c3_private :: PairWordIsOpened
                                 , c4_private :: PairWordIsOpened
                                 , c_private :: PairWordIsOpened
                                 , d1_private :: PairWordIsOpened
                                 , d2_private :: PairWordIsOpened
                                 , d3_private :: PairWordIsOpened
                                 , d4_private :: PairWordIsOpened
                                 , d_private :: PairWordIsOpened
                                 , final_private :: PairWordIsOpened
                                 } deriving (Data, Typeable)

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
                     } deriving (Data, Typeable)

instance Show Association where
    show associationObject = "Association{"
                                          ++ "\n\ta1: " ++ (show $ getItem "a1" associationObject) ++ ","
                                          ++ "\n\ta2: " ++ (show $ getItem "a2" associationObject) ++ ","
                                          ++ "\n\ta3: " ++ (show $ getItem "a3" associationObject) ++ ","
                                          ++ "\n\ta4: " ++ (show $ getItem "a4" associationObject) ++ ","
                                          ++ "\n\ta: " ++ (show $ getItem "a" associationObject) ++ ","
                                          ++ "\n\tb1: " ++ (show $ getItem "b1" associationObject) ++ ","
                                          ++ "\n\tb2: " ++ (show $ getItem "b2" associationObject) ++ ","
                                          ++ "\n\tb3: " ++ (show $ getItem "b3" associationObject) ++ ","
                                          ++ "\n\tb4: " ++ (show $ getItem "b4" associationObject) ++ ","
                                          ++ "\n\tb: " ++ (show $ getItem "b" associationObject) ++ ","
                                          ++ "\n\tc1: " ++ (show $ getItem "c1" associationObject) ++ ","
                                          ++ "\n\tc2: " ++ (show $ getItem "c2" associationObject) ++ ","
                                          ++ "\n\tc3: " ++ (show $ getItem "c3" associationObject) ++ ","
                                          ++ "\n\tc4: " ++ (show $ getItem "c4" associationObject) ++ ","
                                          ++ "\n\tc: " ++ (show $ getItem "c" associationObject) ++ ","
                                          ++ "\n\td1: " ++ (show $ getItem "d1" associationObject) ++ ","
                                          ++ "\n\td2: " ++ (show $ getItem "d2" associationObject) ++ ","
                                          ++ "\n\td3: " ++ (show $ getItem "d3" associationObject) ++ ","
                                          ++ "\n\td4: " ++ (show $ getItem "d4" associationObject) ++ ","
                                          ++ "\n\td: " ++ (show $ getItem "d" associationObject) ++ ","
                                          ++ "\n\tfinal: " ++ (show $ getItem "final" associationObject)
                                          ++ "\n}"


getItem :: String -> Association -> PairWordIsOpened
getItem item associationObject 
    | item == "a1" = a1_private associationObject
    | item == "a2" = a2_private associationObject
    | item == "a3" = a3_private associationObject
    | item == "a4" = a4_private associationObject
    | item == "a" = a_private associationObject
    | item == "b1" = b1_private associationObject
    | item == "b2" = b2_private associationObject
    | item == "b3" = b3_private associationObject
    | item == "b4" = b4_private associationObject
    | item == "c" = c_private associationObject
    | item == "c1" = c1_private associationObject
    | item == "c2" = c2_private associationObject
    | item == "c3" = c3_private associationObject
    | item == "c4" = c4_private associationObject
    | item == "c" = c_private associationObject
    | item == "d1" = d1_private associationObject
    | item == "d2" = d2_private associationObject
    | item == "d3" = d3_private associationObject
    | item == "d4" = d4_private associationObject
    | item == "d" = d_private associationObject
    | item == "final" = final_private associationObject
    | otherwise = PairWordIsOpened{word="", isOpened=False} --die "Bad function call. Function name: getItem, from source file load.hs."
     
setItem :: String -> String -> Bool -> Association -> Association
setItem pair word_ isOpened_ associationObject
    | pair == "a1" = associationObject{a1_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "a2" = associationObject{a2_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "a3" = associationObject{a3_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "a4" = associationObject{a4_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "a" = associationObject{a_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "b1" = associationObject{b1_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "b2" = associationObject{b2_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "b3" = associationObject{b3_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "b4" = associationObject{b4_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "b" = associationObject{b_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "c1" = associationObject{c1_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "c2" = associationObject{c2_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "c3" = associationObject{c3_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "c4" = associationObject{c4_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "c" = associationObject{c_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "d1" = associationObject{d1_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "d2" = associationObject{d2_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "d3" = associationObject{d3_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "d4" = associationObject{d4_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "d" = associationObject{d_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | pair == "final" = associationObject{final_private=PairWordIsOpened{word=word_, isOpened=isOpened_}}
    | otherwise = associationObject

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
    return Association  { a1_private = PairWordIsOpened {word=a1 associationOnlyObject, isOpened=False}
                        , a2_private = PairWordIsOpened {word=a2 associationOnlyObject, isOpened=False}
                        , a3_private = PairWordIsOpened {word=a3 associationOnlyObject, isOpened=False}
                        , a4_private = PairWordIsOpened {word=a4 associationOnlyObject, isOpened=False}
                        , a_private = PairWordIsOpened {word=a associationOnlyObject, isOpened=False}
                        , b1_private = PairWordIsOpened {word=b1 associationOnlyObject, isOpened=False}
                        , b2_private = PairWordIsOpened {word=b2 associationOnlyObject, isOpened=False}
                        , b3_private = PairWordIsOpened {word=b3 associationOnlyObject, isOpened=False}
                        , b4_private = PairWordIsOpened {word=b4 associationOnlyObject, isOpened=False}
                        , b_private = PairWordIsOpened {word=b associationOnlyObject, isOpened=False}
                        , c1_private = PairWordIsOpened {word=c1 associationOnlyObject, isOpened=False}
                        , c2_private = PairWordIsOpened {word=c2 associationOnlyObject, isOpened=False}
                        , c3_private = PairWordIsOpened {word=c3 associationOnlyObject, isOpened=False}
                        , c4_private = PairWordIsOpened {word=c4 associationOnlyObject, isOpened=False}
                        , c_private = PairWordIsOpened {word=c associationOnlyObject, isOpened=False}
                        , d1_private = PairWordIsOpened {word=d1 associationOnlyObject, isOpened=False}
                        , d2_private = PairWordIsOpened {word=d2 associationOnlyObject, isOpened=False}
                        , d3_private = PairWordIsOpened {word=d3 associationOnlyObject, isOpened=False}
                        , d4_private = PairWordIsOpened {word=d4 associationOnlyObject, isOpened=False}
                        , d_private = PairWordIsOpened {word=d associationOnlyObject, isOpened=False}
                        , final_private = PairWordIsOpened {word=final associationOnlyObject, isOpened=False}
                        }    

