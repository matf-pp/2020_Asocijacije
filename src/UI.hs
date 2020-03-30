{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module UI where

import qualified Logic

import qualified Data.Text.IO as T
import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base


createUI :: Maybe [Text] -> IO ()
createUI args = do
    Gtk.init args
    builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"

    Just window <- Logic.getBuilderObj builder "window" Gtk.Window
    on window #destroy $ Logic.printQuit "windows close button"

    Logic.connectBtnClick builder "uiButtonPlayTwoPlayers" $ do 
        Logic.uiButtonPlayTwoPlayersClickHandler builder
        
    Logic.connectBtnClick builder "uiButtonSettings" $ do 
        Logic.uiButtonSettingsClickHandler builder
        
    Logic.connectBtnClick builder "uiButtonBackFromSettings" $ do 
        Logic.uiButtonBackFromSettingsClickHandler builder
      
    Gtk.main
