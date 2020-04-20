{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module UI where

import Logic
import Types

import qualified Data.Text.IO as T
import Data.Text
import Data.Foldable 

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


-- fixBackgroundColorForFixedWidgets :: Gtk.Builder -> IO ()
-- fixBackgroundColorForFixedWidgets builder = do
--     Just uiPlayer1Box <- getBuilderObj builder "uiPlayer1Box" Gtk.Fixed
--     Gtk.set uiPlayer1Box [ HasWindow := True ]

createUI :: Maybe [Text] -> IO ()
createUI args = do
    Gtk.init args
    builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"
    
    loadCss

    -- fixBackgroundColorForFixedWidgets builder

    Just window <- getBuilderObj builder "uiWindow" Gtk.Window
    on window #destroy $ Gtk.mainQuit


    connectBtnClick builder "uiButtonPlayOnePlayer" $ do 
        uiButtonPlayOnePlayerClickHandler builder

    connectBtnClick builder "uiButtonPlayTwoPlayers" $ do 
        uiButtonPlayTwoPlayersClickHandler builder

    traverse_ (\x -> connectBtnClick builder (pack $ poljeId $ x) $ kolonaPoljeButtonHandler x builder) 
                [(NijeKonacno (x, y))  | x <- [A .. D], y <- [F1 .. F4]]

    traverse_ (\x -> connectBtnClick builder (pack $ poljeId $ Konacno $ Just x) $ kolonaHandler x builder)  [A .. D]

    connectEntryActivate builder "uiFinalAnswerEntry"  $ do
        uiFinalAnswerEntry_handler builder

    connectBtnClick builder "uiButtonSettings" $ do 
        uiButtonSettingsClickHandler builder
        
    connectBtnClick builder "uiButtonBackFromSettings" $ do 
        uiButtonBackFromSettingsClickHandler builder
        
    connectBtnClick builder "uiButtonQuit" $ do 
        Gtk.mainQuit
      
    Gtk.main


poljeId :: Polje -> String
poljeId (NijeKonacno (A,F1)) = "ui_A1_Button"
poljeId (NijeKonacno (A,F2)) = "ui_A2_Button"
poljeId (NijeKonacno (A,F3)) = "ui_A3_Button"
poljeId (NijeKonacno (A,F4)) = "ui_A4_Button"
poljeId (NijeKonacno (B,F1)) = "ui_B1_Button"
poljeId (NijeKonacno (B,F2)) = "ui_B2_Button"
poljeId (NijeKonacno (B,F3)) = "ui_B3_Button"
poljeId (NijeKonacno (B,F4)) = "ui_B4_Button"
poljeId (NijeKonacno (C,F1)) = "ui_C1_Button"
poljeId (NijeKonacno (C,F2)) = "ui_C2_Button"
poljeId (NijeKonacno (C,F3)) = "ui_C3_Button"
poljeId (NijeKonacno (C,F4)) = "ui_C4_Button"
poljeId (NijeKonacno (D,F1)) = "ui_D1_Button"
poljeId (NijeKonacno (D,F2)) = "ui_D2_Button"
poljeId (NijeKonacno (D,F3)) = "ui_D3_Button"
poljeId (NijeKonacno (D,F4)) = "ui_D4_Button"
poljeId (Konacno (Just A))   = "uiColumn_A_Entry"
poljeId (Konacno (Just B))   = "uiColumn_B_Entry"
poljeId (Konacno (Just C))   = "uiColumn_C_Entry"
poljeId (Konacno (Just D))   = "uiColumn_D_Entry"
poljeId (Konacno Nothing)    = "uiFinalAnswerEntry"