{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module UI where

import qualified Logic

import qualified Data.Text.IO as T
import Data.Text

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
-- import qualified GI.GLib
import Data.GI.Base


loadCss :: IO ()
loadCss = do
    maybeScreen <- Gdk.screenGetDefault
    provider <- Gtk.cssProviderNew

    case (maybeScreen) of
        (Just screen) -> do
            Gtk.cssProviderLoadFromPath provider (Data.Text.pack "src/resources/style.css")
            Gtk.styleContextAddProviderForScreen
                screen
                provider
                (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
    return ()


createUI :: Maybe [Text] -> IO ()
createUI args = do
    Gtk.init args
    builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"

    Just window <- Logic.getBuilderObj builder "uiWindow" Gtk.Window
    on window #destroy $ Gtk.mainQuit

    loadCss

    Logic.connectBtnClick builder "uiButtonPlayOnePlayer" $ do 
        Logic.uiButtonPlayOnePlayerClickHandler builder

    Logic.connectBtnClick builder "uiButtonPlayTwoPlayers" $ do 
        Logic.uiButtonPlayTwoPlayersClickHandler builder
        
    Logic.connectBtnClick builder "uiButtonSettings" $ do 
        Logic.uiButtonSettingsClickHandler builder
        
    Logic.connectBtnClick builder "uiButtonBackFromSettings" $ do 
        Logic.uiButtonBackFromSettingsClickHandler builder
        
    Logic.connectBtnClick builder "uiButtonQuit" $ do 
        Gtk.mainQuit
      
    Gtk.main
