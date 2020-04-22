{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module UI where

import Logic
import Types

import qualified Data.Text as T
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
            Gtk.cssProviderLoadFromPath provider (T.pack "src/resources/style.css")
            Gtk.styleContextAddProviderForScreen
                screen
                provider
                (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
    return ()


-- fixBackgroundColorForFixedWidgets :: Gtk.Builder -> IO ()
-- fixBackgroundColorForFixedWidgets builder = do
--     Just uiPlayer1Box <- getBuilderObj builder "uiPlayer1Box" Gtk.Fixed
--     Gtk.set uiPlayer1Box [ HasWindow := True ]

createUI :: Maybe [T.Text] -> IO ()
createUI args = do
    Gtk.init args
    builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"
    
    loadCss

    -- fixBackgroundColorForFixedWidgets builder

    Just window <- getBuilderObj builder "uiWindow" Gtk.Window
    on window #destroy $ Gtk.mainQuit

    a1Button' <- getBuilderObj builder (T.pack "ui_A1_Button") Gtk.Button 
    a2Button' <- getBuilderObj builder (T.pack "ui_A2_Button") Gtk.Button
    a3Button' <- getBuilderObj builder (T.pack "ui_A3_Button") Gtk.Button
    a4Button' <- getBuilderObj builder (T.pack "ui_A4_Button") Gtk.Button
    aEntry'   <- getBuilderObj builder (T.pack "uiColumn_A_Entry") Gtk.Entry
    b1Button' <- getBuilderObj builder (T.pack "ui_B1_Button") Gtk.Button
    b2Button' <- getBuilderObj builder (T.pack "ui_B2_Button") Gtk.Button
    b3Button' <- getBuilderObj builder (T.pack "ui_B3_Button") Gtk.Button
    b4Button' <- getBuilderObj builder (T.pack "ui_B4_Button") Gtk.Button
    bEntry'   <- getBuilderObj builder (T.pack "uiColumn_B_Entry") Gtk.Entry
    c1Button' <- getBuilderObj builder (T.pack "ui_C1_Button") Gtk.Button
    c2Button' <- getBuilderObj builder (T.pack "ui_C2_Button") Gtk.Button
    c3Button' <- getBuilderObj builder (T.pack "ui_C3_Button") Gtk.Button
    c4Button' <- getBuilderObj builder (T.pack "ui_C4_Button") Gtk.Button
    cEntry'   <- getBuilderObj builder (T.pack "uiColumn_C_Entry") Gtk.Entry
    d1Button' <- getBuilderObj builder (T.pack "ui_D1_Button") Gtk.Button
    d2Button' <- getBuilderObj builder (T.pack "ui_D2_Button") Gtk.Button
    d3Button' <- getBuilderObj builder (T.pack "ui_D3_Button") Gtk.Button
    d4Button' <- getBuilderObj builder (T.pack "ui_D4_Button") Gtk.Button
    dEntry'   <- getBuilderObj builder (T.pack "uiColumn_D_Entry") Gtk.Entry
    finEntry' <- getBuilderObj builder (T.pack "uiFinalAnswerEntry") Gtk.Entry
    startGameButton' <- getBuilderObj builder (T.pack "uiButtonPlayTwoPlayers") Gtk.Button
    settingsButton' <- getBuilderObj builder (T.pack "uiButtonSettings") Gtk.Button
    backButton' <- getBuilderObj builder (T.pack "uiButtonBackFromSettings") Gtk.Button
    nextButton' <- getBuilderObj builder (T.pack "uiButtonNext") Gtk.Button
    quitButton' <- getBuilderObj builder (T.pack "uiButtonQuit") Gtk.Button

    saveUI   UI { a1Button = a1Button', 
                  a2Button = a2Button', 
                  a3Button = a3Button', 
                  a4Button = a4Button', 
                  aEntry   = aEntry', 
                  b1Button = b1Button', 
                  b2Button = b2Button', 
                  b3Button = b3Button', 
                  b4Button = b4Button', 
                  bEntry   = bEntry', 
                  c1Button = c1Button', 
                  c2Button = c2Button', 
                  c3Button = c3Button', 
                  c4Button = c4Button', 
                  cEntry   = cEntry', 
                  d1Button = d1Button', 
                  d2Button = d2Button', 
                  d3Button = d3Button', 
                  d4Button = d4Button', 
                  dEntry   = dEntry', 
                  finEntry = finEntry',
                  startGameButton = startGameButton',
                  settingsButton = settingsButton', 
                  backButton = backButton',
                  nextButton = nextButton',
                  quitButton = quitButton'}

    connectBtnClick (startGameButton loadUI) $ uiButtonPlayTwoPlayersClickHandler builder
    connectBtnClick (settingsButton loadUI) $ uiButtonSettingsClickHandler builder
    connectBtnClick (backButton loadUI) $ uiButtonBackFromSettingsClickHandler builder
 -- connectBtnClick (nextButton loadUI) $ poljeHandler (NijeKonacno (D, F4))
    connectBtnClick (quitButton loadUI) $ Gtk.mainQuit

    traverse_ (\x -> connectBtnClick (poljeButton x) $ poljeHandler x) [(x,y) | x <- [A .. D], y <- [F1 .. F4]]
    traverse_ (\x -> connectEntryActivate' (kolonaEntry (Just x)) $ kolonaHandler x builder) [A .. D]
    connectEntryActivate' (finEntry loadUI) $ uiFinalAnswerEntry_handler builder

    Gtk.main
