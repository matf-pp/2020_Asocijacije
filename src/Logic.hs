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
    putStrLn $ show $ GameState.getSettings gameStateObject
    
    Just uiPlayer1NameLabel <- getBuilderObj builder "uiPlayer1NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer1NameLabel $ T.pack $ LoadSettings.getItem "player1_name" $ GameState.getSettings gameStateObject
    Just uiPlayer2NameLabel <- getBuilderObj builder "uiPlayer2NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer2NameLabel $ T.pack $ LoadSettings.getItem "player2_name" $ GameState.getSettings gameStateObject

    Just uiPlayer1ScoreLabel <- getBuilderObj builder "uiPlayer1ScoreLabel" Gtk.Label
    Gtk.labelSetText uiPlayer1ScoreLabel $ T.pack $ show $ GameState.player1_score gameStateObject 
    Just uiPlayer2ScoreLabel <- getBuilderObj builder "uiPlayer2ScoreLabel" Gtk.Label
    Gtk.labelSetText uiPlayer2ScoreLabel $ T.pack $ show $ GameState.player2_score gameStateObject

    setFirstPlayerToPlay gameStateObject builder

 
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
    Just uiComboBoxText_first_play <- getBuilderObj builder "uiComboBoxText_first_play" Gtk.ComboBoxText
    Gtk.entrySetText uiEntry_player1_name $ T.pack $ LoadSettings.getItem "player1_name" settingsObject
    Gtk.entrySetText uiEntry_player1_color $ T.pack $ LoadSettings.getItem "player1_color" settingsObject
    Gtk.entrySetText uiEntry_player1_image $ T.pack $ LoadSettings.getItem "player1_image" settingsObject
    Gtk.entrySetText uiEntry_player2_name $ T.pack $ LoadSettings.getItem "player2_name" settingsObject
    Gtk.entrySetText uiEntry_player2_color $ T.pack $ LoadSettings.getItem "player2_color" settingsObject
    Gtk.entrySetText uiEntry_player2_image $ T.pack $ LoadSettings.getItem "player2_image" settingsObject
    Gtk.entrySetText uiEntry_game_duration_in_seconds $ T.pack $ LoadSettings.getItem "game_duration_in_seconds" settingsObject
    Gtk.entrySetText uiEntry_waiting_time_for_one_play_in_seconds $ T.pack $ LoadSettings.getItem "waiting_time_for_one_play_in_seconds" settingsObject
    let indexIntProcitan = (read $ LoadSettings.getItem "first_play" settingsObject) 
    let index = if ((indexIntProcitan == 1) || (indexIntProcitan == 2)) then indexIntProcitan - 1 else 0
    Gtk.comboBoxSetActive uiComboBoxText_first_play index



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
      
    Just uiComboBoxText_first_play <- getBuilderObj builder "uiComboBoxText_first_play" Gtk.ComboBoxText
    uiComboBoxText_first_play_text <- Gtk.comboBoxTextGetActiveText uiComboBoxText_first_play
    let uiComboBoxText_first_play_str = T.unpack uiComboBoxText_first_play_text
    let indexInt = if uiComboBoxText_first_play_str == "Igrač 2" then 2 else 1
    let index = show indexInt
    let ui_first_play_str = index

    let settingsObject = LoadSettings.Settings uiEntry_player1_name_str uiEntry_player1_color_str uiEntry_player1_image_str uiEntry_player2_name_str uiEntry_player2_color_str uiEntry_player2_image_str uiEntry_game_duration_in_seconds_str uiEntry_waiting_time_for_one_play_in_seconds_str ui_first_play_str
    LoadSettings.writeToSettingsFile settingsObject
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiMainMenu <- getBuilderObj builder "uiMainMenu" Gtk.Box
    Gtk.stackSetVisibleChild uiStack uiMainMenu

    

ui_abcd1234_ButtonHandler :: String -> Gtk.Builder -> IO ()
ui_abcd1234_ButtonHandler button_id builder = do
    --putStrLn $ kojeJeDugme button_id
    gameStateObject <- GameState.loadGameState
    if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (kojeJeDugme button_id) $ GameState.getAssociation gameStateObject) == False then do
        gameStateObject <- upisiRec button_id gameStateObject builder
        gameStateObject <- changePlayerOnMove gameStateObject builder
        putStrLn $ show $ gameStateObject
        GameState.saveGameState gameStateObject

    else 
        return ()

changePlayerOnMove :: GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
changePlayerOnMove gameStateObject builder = do
    if GameState.on_move gameStateObject == 1 then do
        setPlayer2ToPlay gameStateObject builder
    else if GameState.on_move gameStateObject == 2 then do
        setPlayer1ToPlay gameStateObject builder
    else 
        return (gameStateObject)


 

setPlayer1ToPlay :: GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
setPlayer1ToPlay gameStateObject builder = do
    Just uiPlayer1BoxEventBox <- getBuilderObj builder "uiPlayer1BoxEventBox" Gtk.EventBox
    styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer1BoxEventBox
    Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"

    Just senka1 <- getBuilderObj builder "senka1" Gtk.Label
    styleContextSenka1 <- Gtk.widgetGetStyleContext senka1
    Gtk.styleContextAddClass styleContextSenka1 $ T.pack "na-potezu"

    Just uiPlayer2BoxEventBox <- getBuilderObj builder "uiPlayer2BoxEventBox" Gtk.EventBox
    styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer2BoxEventBox
    Gtk.styleContextRemoveClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"

    Just senka2 <- getBuilderObj builder "senka2" Gtk.Label
    styleContextSenka2 <- Gtk.widgetGetStyleContext senka2
    Gtk.styleContextRemoveClass styleContextSenka2 $ T.pack "na-potezu"

    return (gameStateObject{GameState.on_move = 1})


setPlayer2ToPlay :: GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
setPlayer2ToPlay gameStateObject builder = do
    Just uiPlayer2BoxEventBox <- getBuilderObj builder "uiPlayer2BoxEventBox" Gtk.EventBox
    styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer2BoxEventBox
    Gtk.styleContextAddClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"

    Just senka2 <- getBuilderObj builder "senka2" Gtk.Label
    styleContextSenka2 <- Gtk.widgetGetStyleContext senka2
    Gtk.styleContextAddClass styleContextSenka2 $ T.pack "na-potezu"

    Just uiPlayer1BoxEventBox <- getBuilderObj builder "uiPlayer1BoxEventBox" Gtk.EventBox
    styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer1BoxEventBox
    Gtk.styleContextRemoveClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"

    Just senka1 <- getBuilderObj builder "senka1" Gtk.Label
    styleContextSenka1 <- Gtk.widgetGetStyleContext senka1
    Gtk.styleContextRemoveClass styleContextSenka1 $ T.pack "na-potezu"

    return (gameStateObject{GameState.on_move = 2})


setFirstPlayerToPlay :: GameState.GameState -> Gtk.Builder -> IO ()
setFirstPlayerToPlay gameStateObject builder = do
    if GameState.on_move gameStateObject == 1 then do
        Just uiPlayer1BoxEventBox <- getBuilderObj builder "uiPlayer1BoxEventBox" Gtk.EventBox
        styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer1BoxEventBox
        Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"

        Just senka1 <- getBuilderObj builder "senka1" Gtk.Label
        styleContextSenka1 <- Gtk.widgetGetStyleContext senka1
        Gtk.styleContextAddClass styleContextSenka1 $ T.pack "na-potezu"
    else if GameState.on_move gameStateObject == 2 then do
        Just uiPlayer2BoxEventBox <- getBuilderObj builder "uiPlayer2BoxEventBox" Gtk.EventBox
        styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext uiPlayer2BoxEventBox
        Gtk.styleContextAddClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"

        Just senka2 <- getBuilderObj builder "senka2" Gtk.Label
        styleContextSenka2 <- Gtk.widgetGetStyleContext senka2
        Gtk.styleContextAddClass styleContextSenka2 $ T.pack "na-potezu"
    else 
        return ()


upisiRec :: String -> GameState.GameState -> Gtk.Builder -> IO(GameState.GameState)
upisiRec button_id gameStateObject builder = do
    Just uiButton <- getBuilderObj builder (T.pack button_id) Gtk.Button
    let word = LoadAssociation.getWord $ LoadAssociation.getItem (kojeJeDugme button_id) $ GameState.getAssociation gameStateObject
    Gtk.buttonSetLabel uiButton $ T.pack word
    return (gameStateObject{GameState.association = LoadAssociation.setItem (kojeJeDugme button_id) word True $ GameState.getAssociation gameStateObject})

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

