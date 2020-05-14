{-# LANGUAGE DeriveDataTypeable #-}
module LoadAssociation 
(
    makeNewAssociation,
) where

import Types

import Text.JSON.Generic
import System.Environment  
import System.IO
import System.Directory
import System.Random


data AssociationOnly = AssociationOnly
                     { a1 :: String 
                     , a2 :: String
                     , a3 :: String
                     , a4 :: String
                     , a  :: String
                     , b1 :: String
                     , b2 :: String
                     , b3 :: String
                     , b4 :: String
                     , b  :: String
                     , c1 :: String
                     , c2 :: String
                     , c3 :: String
                     , c4 :: String
                     , c  :: String
                     , d1 :: String
                     , d2 :: String
                     , d3 :: String
                     , d4 :: String
                     , d  :: String
                     , final :: String
                     } deriving (Data, Typeable, Show)


makeFileName :: IO FilePath
makeFileName = do
    size <- fmap length $ listDirectory directoryWithAssociations
    randomNum <- getStdRandom $ randomR (1, size) 
    return $ directoryWithAssociations ++ "asocijacija" ++ (show randomNum) ++ ".json"
    where directoryWithAssociations = "association/"

   
makeNewAssociation :: IO (Association)
makeNewAssociation = do
    associationOnlyJson <- makeFileName >>= readFile
    associationOnly <- return (decodeJSON associationOnlyJson :: AssociationOnly)
    return   Association{ a1_private = (a1 associationOnly, False)
                        , a2_private = (a2 associationOnly, False)
                        , a3_private = (a3 associationOnly, False)
                        , a4_private = (a4 associationOnly, False)
                        , a_private  = (a  associationOnly,  False)
                        , b1_private = (b1 associationOnly, False)
                        , b2_private = (b2 associationOnly, False)
                        , b3_private = (b3 associationOnly, False)
                        , b4_private = (b4 associationOnly, False)
                        , b_private  = (b  associationOnly,  False)
                        , c1_private = (c1 associationOnly, False)
                        , c2_private = (c2 associationOnly, False)
                        , c3_private = (c3 associationOnly, False)
                        , c4_private = (c4 associationOnly, False)
                        , c_private  = (c  associationOnly,  False)
                        , d1_private = (d1 associationOnly, False)
                        , d2_private = (d2 associationOnly, False)
                        , d3_private = (d3 associationOnly, False)
                        , d4_private = (d4 associationOnly, False)
                        , d_private  = (d  associationOnly,  False)
                        , final_private = (final associationOnly, False)
                        }
