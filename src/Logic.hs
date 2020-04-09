{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Logic 
(
getBuilderObj,
connectBtnClick,
uiButtonPlayOnePlayerClickHandler,
uiButtonPlayTwoPlayersClickHandler,
uiButtonSettingsClickHandler,
uiButtonBackFromSettingsClickHandler,
ui_abcd1234_ButtonHandler
) where
    
import qualified LoadSettings
import qualified LoadAssociation
import qualified GameState

import Data.Monoid ((<>))
import qualified Data.Text.IO as T_IO
import qualified Data.Text as T
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import Data.GI.Base

getBuilderObj :: forall o'
               . GObject o' 
               => Gtk.Builder 
               -> Text 
               -> (ManagedPtr o' -> o') 
               -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
    Just obj -> castTo gtkConstr obj
    Nothing -> do
        T_IO.putStrLn $ "Object named '" <> name <> "' could not be found."
        return Nothing

connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
connectBtnClick builder name handler = getBuilderObj builder name Gtk.Button >>= \case
    Just button -> do 
        on button #clicked $ do handler
        return ()
    Nothing -> return ()

uiButtonPlayOnePlayerClickHandler :: Gtk.Builder -> IO ()
uiButtonPlayOnePlayerClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiOnePlayerGame <- getBuilderObj builder "uiOnePlayerGame" Gtk.Grid
    Gtk.stackSetVisibleChild uiStack uiOnePlayerGame 

uiButtonPlayTwoPlayersClickHandler :: Gtk.Builder -> IO ()
uiButtonPlayTwoPlayersClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiTwoPlayersGame <- getBuilderObj builder "uiTwoPlayersGameEventBox" Gtk.EventBox
    Gtk.stackSetVisibleChild uiStack uiTwoPlayersGame 
    GameState.makeGameState
    gameStateObject <- GameState.loadGameState
    --putStrLn $ show $ GameState.getSettings gameStateObject
    Just uiPlayer1NameLabel <- getBuilderObj builder "uiPlayer1NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer1NameLabel $ T.pack $ LoadSettings.getItem "player1_name" $ GameState.getSettings gameStateObject
    Just uiPlayer2NameLabel <- getBuilderObj builder "uiPlayer2NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer2NameLabel $ T.pack $ LoadSettings.getItem "player2_name" $ GameState.getSettings gameStateObject

 
uiButtonSettingsClickHandler :: Gtk.Builder -> IO ()
uiButtonSettingsClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiSettings <- getBuilderObj builder "uiSettings" Gtk.Box
    Gtk.stackSetVisibleChild uiStack uiSettings
    settingsObject <- LoadSettings.readSettingsFile
    putStrLn $ show $ settingsObject
    -- previse imperativno, uradi na funkcionalni nacin
    Just uiEntry_player1_name <- getBuilderObj builder "uiEntry_player1_name" Gtk.Entry
    Just uiEntry_player1_color <- getBuilderObj builder "uiEntry_player1_color" Gtk.Entry
    Just uiEntry_player1_image <- getBuilderObj builder "uiEntry_player1_image" Gtk.Entry
    Just uiEntry_player2_name <- getBuilderObj builder "uiEntry_player2_name" Gtk.Entry
    Just uiEntry_player2_color <- getBuilderObj builder "uiEntry_player2_color" Gtk.Entry
    Just uiEntry_player2_image <- getBuilderObj builder "uiEntry_player2_image" Gtk.Entry
    Just uiEntry_game_duration_in_seconds <- getBuilderObj builder "uiEntry_game_duration_in_seconds" Gtk.Entry
    Just uiEntry_waiting_time_for_one_play_in_seconds <- getBuilderObj builder "uiEntry_waiting_time_for_one_play_in_seconds" Gtk.Entry
    Gtk.entrySetText uiEntry_player1_name $ T.pack $ LoadSettings.getItem "player1_name" settingsObject
    Gtk.entrySetText uiEntry_player1_color $ T.pack $ LoadSettings.getItem "player1_color" settingsObject
    Gtk.entrySetText uiEntry_player1_image $ T.pack $ LoadSettings.getItem "player1_image" settingsObject
    Gtk.entrySetText uiEntry_player2_name $ T.pack $ LoadSettings.getItem "player2_name" settingsObject
    Gtk.entrySetText uiEntry_player2_color $ T.pack $ LoadSettings.getItem "player2_color" settingsObject
    Gtk.entrySetText uiEntry_player2_image $ T.pack $ LoadSettings.getItem "player2_image" settingsObject
    Gtk.entrySetText uiEntry_game_duration_in_seconds $ T.pack $ LoadSettings.getItem "game_duration_in_seconds" settingsObject
    Gtk.entrySetText uiEntry_waiting_time_for_one_play_in_seconds $ T.pack $ LoadSettings.getItem "waiting_time_for_one_play_in_seconds" settingsObject

uiButtonBackFromSettingsClickHandler :: Gtk.Builder -> IO ()
uiButtonBackFromSettingsClickHandler builder = do
    
    Just uiEntry_player1_name <- getBuilderObj builder "uiEntry_player1_name" Gtk.Entry
    uiEntry_player1_name_text <- Gtk.getEntryText uiEntry_player1_name
    let uiEntry_player1_name_str = T.unpack uiEntry_player1_name_text
    
    Just uiEntry_player1_color <- getBuilderObj builder "uiEntry_player1_color" Gtk.Entry
    uiEntry_player1_color_text <- Gtk.getEntryText uiEntry_player1_color
    let uiEntry_player1_color_str = T.unpack uiEntry_player1_color_text
    
    Just uiEntry_player1_image <- getBuilderObj builder "uiEntry_player1_image" Gtk.Entry
    uiEntry_player1_image_text <- Gtk.getEntryText uiEntry_player1_image
    let uiEntry_player1_image_str = T.unpack uiEntry_player1_image_text
    
    Just uiEntry_player2_name <- getBuilderObj builder "uiEntry_player2_name" Gtk.Entry
    uiEntry_player2_name_text <- Gtk.getEntryText uiEntry_player2_name
    let uiEntry_player2_name_str = T.unpack uiEntry_player2_name_text
    
    Just uiEntry_player2_color <- getBuilderObj builder "uiEntry_player2_color" Gtk.Entry
    uiEntry_player2_color_text <- Gtk.getEntryText uiEntry_player2_color
    let uiEntry_player2_color_str = T.unpack uiEntry_player2_color_text
    
    Just uiEntry_player2_image <- getBuilderObj builder "uiEntry_player2_image" Gtk.Entry
    uiEntry_player2_image_text <- Gtk.getEntryText uiEntry_player2_image
    let uiEntry_player2_image_str = T.unpack uiEntry_player2_image_text
    
    Just uiEntry_game_duration_in_seconds <- getBuilderObj builder "uiEntry_game_duration_in_seconds" Gtk.Entry
    uiEntry_game_duration_in_seconds_text <- Gtk.getEntryText uiEntry_game_duration_in_seconds
    let uiEntry_game_duration_in_seconds_str = T.unpack uiEntry_game_duration_in_seconds_text
    
    Just uiEntry_waiting_time_for_one_play_in_seconds <- getBuilderObj builder "uiEntry_waiting_time_for_one_play_in_seconds" Gtk.Entry
    uiEntry_waiting_time_for_one_play_in_seconds_text <- Gtk.getEntryText uiEntry_waiting_time_for_one_play_in_seconds
    let uiEntry_waiting_time_for_one_play_in_seconds_str = T.unpack uiEntry_waiting_time_for_one_play_in_seconds_text
      
    let settingsObject = LoadSettings.Settings uiEntry_player1_name_str uiEntry_player1_color_str uiEntry_player1_image_str uiEntry_player2_name_str uiEntry_player2_color_str uiEntry_player2_image_str uiEntry_game_duration_in_seconds_str uiEntry_waiting_time_for_one_play_in_seconds_str
    LoadSettings.writeToSettingsFile settingsObject
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiMainMenu <- getBuilderObj builder "uiMainMenu" Gtk.Box
    Gtk.stackSetVisibleChild uiStack uiMainMenu

ui_abcd1234_ButtonHandler :: String -> Gtk.Builder -> IO ()
ui_abcd1234_ButtonHandler button_id builder = do
    --putStrLn $ kojeJeDugme button_id
    gameStateObject <- GameState.loadGameState
    Just uiButton <- getBuilderObj builder (T.pack button_id) Gtk.Button
    if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (kojeJeDugme button_id) $ GameState.getAssociation gameStateObject) == False then do
        let word = LoadAssociation.getWord $ LoadAssociation.getItem (kojeJeDugme button_id) $ GameState.getAssociation gameStateObject
        Gtk.buttonSetLabel uiButton $ T.pack word
        putStrLn $ show $ gameStateObject{GameState.association = LoadAssociation.setItem (kojeJeDugme button_id) word True $ GameState.getAssociation gameStateObject}
        GameState.saveGameState $ gameStateObject{GameState.association = LoadAssociation.setItem (kojeJeDugme button_id) word True $ GameState.getAssociation gameStateObject}
    else 
        return ()
          

kojeJeDugme :: String -> String
kojeJeDugme id
    | id == "ui_A1_Button" = "a1"
    | id == "ui_A2_Button" = "a2"
    | id == "ui_A3_Button" = "a3"
    | id == "ui_A4_Button" = "a4"
    | id == "ui_B1_Button" = "b1"
    | id == "ui_B2_Button" = "b2"
    | id == "ui_B3_Button" = "b3"
    | id == "ui_B4_Button" = "b4"
    | id == "ui_C1_Button" = "c1"
    | id == "ui_C2_Button" = "c2"
    | id == "ui_C3_Button" = "c3"
    | id == "ui_C4_Button" = "c4"
    | id == "ui_D1_Button" = "d1"
    | id == "ui_D2_Button" = "d2"
    | id == "ui_D3_Button" = "d3"
    | id == "ui_D4_Button" = "d4"
    | otherwise = ""

