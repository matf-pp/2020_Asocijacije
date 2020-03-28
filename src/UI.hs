{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module UI where

import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base

printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

getBuilderObj :: forall o'
               . GObject o' 
               => Gtk.Builder 
               -> Text 
               -> (ManagedPtr o' -> o') 
               -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing

connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
connectBtnClick builder name handler = getBuilderObj builder name Gtk.Button >>= \case
  Just button -> do 
    on button #clicked $ do handler
    return ()
  Nothing -> return ()
  
stampaj :: [Char] -> IO()
stampaj ime = putStrLn $ ime
  
createUI :: Maybe [Text] -> IO ()
createUI args = do
  Gtk.init args
  builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"

  Just window <- getBuilderObj builder "window" Gtk.Window
  on window #destroy $ printQuit "windows close button"
  
  connectBtnClick builder "uiButtonPlayTwoPlayers" $ do 
      stampaj "Igraj"
      
  connectBtnClick builder "uiButtonSettings" $ do 
      stampaj "Podesavanja"

  Gtk.main
