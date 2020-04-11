{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Logic 
(
getBuilderObj,
connectBtnClick,
connectEntryActivate,
uiButtonPlayOnePlayerClickHandler,
uiButtonPlayTwoPlayersClickHandler,
uiButtonSettingsClickHandler,
uiButtonBackFromSettingsClickHandler,
ui_abcd1234_ButtonHandler,
--uiColumn_A_Entry_handler,
uiColumn_ABCD_Entry_handler,
uiFinalAnswerEntry_handler
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

connectEntryActivate :: Gtk.Builder -> Text -> IO () -> IO ()
connectEntryActivate builder name handler = getBuilderObj builder name Gtk.Entry >>= \case
    Just entry -> do 
        on entry #activate $ do handler
        return ()
    Nothing -> return ()

uiButtonPlayOnePlayerClickHandler :: Gtk.Builder -> IO ()
uiButtonPlayOnePlayerClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiOnePlayerGame <- getBuilderObj builder "uiOnePlayerGame" Gtk.Grid
    Gtk.stackSetVisibleChild uiStack uiOnePlayerGame 

ukloniFokusSaTrenutnogEntry :: Gtk.Builder -> IO ()
ukloniFokusSaTrenutnogEntry builder = do
    Just ui_A1_Button <- getBuilderObj builder "ui_A1_Button" Gtk.Button
    Gtk.widgetSetCanFocus ui_A1_Button True
    Gtk.widgetGrabFocus ui_A1_Button

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

    ukloniFokusSaTrenutnogEntry builder -- fokus na pocetku se sklanja sa polja konacno (sto se desava automatski) na polje a1

 
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
    --putStrLn $ btnIdToPolje button_id
    gameStateObject <- GameState.loadGameState
    if (daLiPoljeNijeOtvorenoIDaLiIgracNaPotezuNijeOtvaraoPolje gameStateObject) then do
        gameStateObject <- upisiRecBtn button_id gameStateObject builder
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        -- DEBUG ISPIS, treba obrisati posle
        gs <- GameState.loadGameState
        putStrLn $ show $ gs
    else 
        return ()
    where daLiPoljeNijeOtvorenoIDaLiIgracNaPotezuNijeOtvaraoPolje gameStateObject = (daLiPoljeNijeOtvoreno gameStateObject) && (daLiIgracNaPotezuNijeOtvaraoPolje gameStateObject)
          daLiPoljeNijeOtvoreno gameStateObject = (LoadAssociation.getIsOpened $ LoadAssociation.getItem (btnIdToPolje button_id) $ GameState.getAssociation gameStateObject) == False
          daLiIgracNaPotezuNijeOtvaraoPolje gameStateObject = (GameState.did_on_move_player_open_word gameStateObject) == False
        

changePlayerOnMove :: GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
changePlayerOnMove gameStateObject builder = do
    if GameState.on_move gameStateObject == 1 then do
        newGameStateObject <- setPlayer2ToPlay gameStateObject builder
        return (newGameStateObject)
    else if GameState.on_move gameStateObject == 2 then do
        newGameStateObject <- setPlayer1ToPlay gameStateObject builder
        return (newGameStateObject)
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

    return (gameStateObject{GameState.on_move = 1, GameState.did_on_move_player_open_word = False})


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

    return (gameStateObject{GameState.on_move = 2, GameState.did_on_move_player_open_word = False})


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


upisiRecBtn :: String -> GameState.GameState -> Gtk.Builder -> IO(GameState.GameState)
upisiRecBtn button_id gameStateObject builder = do
    Just uiButton <- getBuilderObj builder (T.pack button_id) Gtk.Button
    let word = LoadAssociation.getWord $ LoadAssociation.getItem (btnIdToPolje button_id) $ GameState.getAssociation gameStateObject
    Gtk.buttonSetLabel uiButton $ T.pack word
    return (gameStateObject{GameState.association = LoadAssociation.setItem (btnIdToPolje button_id) word True $ GameState.getAssociation gameStateObject})


upisiRecEntry :: String -> GameState.GameState -> Gtk.Builder -> IO(GameState.GameState)
upisiRecEntry entry_id gameStateObject builder = do
    Just uiEntry <- getBuilderObj builder (T.pack entry_id) Gtk.Entry
    let word = LoadAssociation.getWord $ LoadAssociation.getItem (entryIdToColumnCell entry_id "columnFinal") $ GameState.getAssociation gameStateObject
    Gtk.entrySetText uiEntry $ T.pack word
    return (gameStateObject{GameState.association = LoadAssociation.setItem (entryIdToColumnCell entry_id "columnFinal") word True $ GameState.getAssociation gameStateObject})



upisiRec :: String -> GameState.GameState -> Gtk.Builder -> IO(GameState.GameState)
upisiRec polje gameStateObject builder = do
    if any (==True) $ map (==polje) ["a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4", "c1", "c2", "c3", "c4", "d1", "d2", "d3", "d4"] then do
        if (LoadAssociation.getIsOpened $ LoadAssociation.getItem polje $ GameState.getAssociation gameStateObject) == False then do
            gameStateObject <- upisiRecBtn (poljeToButtonId polje) gameStateObject builder
            return (gameStateObject)
            else 
                return (gameStateObject)
        else do
            if (LoadAssociation.getIsOpened $ LoadAssociation.getItem polje $ GameState.getAssociation gameStateObject) == False then do
                gameStateObject <- upisiRecEntry (poljeToEntryId polje) gameStateObject builder
                return (gameStateObject)
                else 
                    return (gameStateObject)


obojiPolje :: String -> GameState.GameState -> Gtk.Builder -> IO()
obojiPolje polje gameStateObject builder = do
    if any (==True) $ map (==polje) ["a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4", "c1", "c2", "c3", "c4", "d1", "d2", "d3", "d4"] then do
        Just uiButton <- getBuilderObj builder (T.pack $ poljeToButtonId polje) Gtk.Button
        styleContextUiButton <- Gtk.widgetGetStyleContext uiButton
        Gtk.styleContextAddClass styleContextUiButton $ T.pack $ "polje-" ++ (LoadSettings.getItem ("player" ++ (show $ GameState.on_move gameStateObject) ++ "_color") $ GameState.getSettings gameStateObject)
    
        else do
            Just uiEntry <- getBuilderObj builder (T.pack $ poljeToEntryId polje) Gtk.Entry
            styleContextUiEntry <- Gtk.widgetGetStyleContext uiEntry
            Gtk.styleContextAddClass styleContextUiEntry $ T.pack $ "polje-" ++ (LoadSettings.getItem ("player" ++ (show $ GameState.on_move gameStateObject) ++ "_color") $ GameState.getSettings gameStateObject)
    

poljeToEntryId :: String -> String
poljeToEntryId polje
    | polje == "a" = "uiColumn_A_Entry"
    | polje == "b" = "uiColumn_B_Entry"
    | polje == "c" = "uiColumn_C_Entry"
    | polje == "d" = "uiColumn_D_Entry"
    | otherwise    = "uiFinalAnswerEntry"

btnIdToPolje :: String -> String
btnIdToPolje id
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

poljeToButtonId :: String -> String
poljeToButtonId polje
    | polje == "a1" = "ui_A1_Button"
    | polje == "a2" = "ui_A2_Button"
    | polje == "a3" = "ui_A3_Button"
    | polje == "a4" = "ui_A4_Button"
    | polje == "b1" = "ui_B1_Button"
    | polje == "b2" = "ui_B2_Button"
    | polje == "b3" = "ui_B3_Button"
    | polje == "b4" = "ui_B4_Button"
    | polje == "c1" = "ui_C1_Button"
    | polje == "c2" = "ui_C2_Button"
    | polje == "c3" = "ui_C3_Button"
    | polje == "c4" = "ui_C4_Button"
    | polje == "d1" = "ui_D1_Button"
    | polje == "d2" = "ui_D2_Button"
    | polje == "d3" = "ui_D3_Button"
    | polje == "d4" = "ui_D4_Button"
    | otherwise = ""

{-uiColumn_A_Entry_handler :: Gtk.Builder -> IO ()
uiColumn_A_Entry_handler builder = do
    Just uiColumn_A_Entry <- getBuilderObj builder "uiColumn_A_Entry" Gtk.Entry
    input_Text <- Gtk.entryGetText uiColumn_A_Entry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = LoadAssociation.getWord $ LoadAssociation.getItem "a" $ GameState.getAssociation gameStateObject
    if user_input == correct_answer then do
        let bonus_poeni = ((a1_bonus gameStateObject)+ (a2_bonus gameStateObject) + (a3_bonus gameStateObject) + (a4_bonus gameStateObject))
        gameStateObject <- upisiRec "a1" gameStateObject builder
        gameStateObject <- upisiRec "a2" gameStateObject builder
        gameStateObject <- upisiRec "a3" gameStateObject builder
        gameStateObject <- upisiRec "a4" gameStateObject builder
        gameStateObject <- dodajPoene (10 + bonus_poeni) gameStateObject builder
        set uiColumn_A_Entry [ #editable := False ]
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        else do
            gameStateObject <- changePlayerOnMove gameStateObject builder
            Gtk.entrySetText uiColumn_A_Entry $ T.pack ""
            GameState.saveGameState gameStateObject
            Just ui_A1_Button <- getBuilderObj builder "ui_A1_Button" Gtk.Button
            Gtk.widgetSetCanFocus ui_A1_Button True
            Gtk.widgetGrabFocus ui_A1_Button

    
    where a1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a1" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a2" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a3" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a4" $ GameState.getAssociation gameStateObject) == False then 2 else 0
-}

uiColumn_ABCD_Entry_handler :: String -> Gtk.Builder -> IO ()
uiColumn_ABCD_Entry_handler entry_id builder = do
    Just uiColumn_ABCD_Entry <- getBuilderObj builder (T.pack entry_id) Gtk.Entry
    input_Text <- Gtk.entryGetText uiColumn_ABCD_Entry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = LoadAssociation.getWord $ LoadAssociation.getItem (entryIdToColumnCell entry_id "columnFinal") $ GameState.getAssociation gameStateObject
    if user_input == correct_answer then do
        let bonus_poeni = ((cell1_bonus gameStateObject)+ (cell2_bonus gameStateObject) + (cell3_bonus gameStateObject) + (cell4_bonus gameStateObject))
        gameStateObject <- upisiRec (entryIdToColumnCell entry_id "cell1") gameStateObject builder
        gameStateObject <- upisiRec (entryIdToColumnCell entry_id "cell2") gameStateObject builder
        gameStateObject <- upisiRec (entryIdToColumnCell entry_id "cell3") gameStateObject builder
        gameStateObject <- upisiRec (entryIdToColumnCell entry_id "cell4") gameStateObject builder
        obojiPolje                  (entryIdToColumnCell entry_id "cell1") gameStateObject builder
        obojiPolje                  (entryIdToColumnCell entry_id "cell2") gameStateObject builder
        obojiPolje                  (entryIdToColumnCell entry_id "cell3") gameStateObject builder
        obojiPolje                  (entryIdToColumnCell entry_id "cell4") gameStateObject builder
        obojiPolje                  (entryIdToColumnCell entry_id "columnFinal") gameStateObject builder
        gameStateObject <- dodajPoene (10 + bonus_poeni) gameStateObject builder
        set uiColumn_ABCD_Entry [ #editable := False ]
        ukloniFokusSaTrenutnogEntry builder 
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        else do
            gameStateObject <- changePlayerOnMove gameStateObject builder
            Gtk.entrySetText uiColumn_ABCD_Entry $ T.pack ""
            GameState.saveGameState gameStateObject
            ukloniFokusSaTrenutnogEntry builder
            

    
    where cell1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (entryIdToColumnCell entry_id "cell1") $ GameState.getAssociation gameStateObject) == False then 2 else 0
          cell2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (entryIdToColumnCell entry_id "cell2") $ GameState.getAssociation gameStateObject) == False then 2 else 0
          cell3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (entryIdToColumnCell entry_id "cell3") $ GameState.getAssociation gameStateObject) == False then 2 else 0
          cell4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem (entryIdToColumnCell entry_id "cell4") $ GameState.getAssociation gameStateObject) == False then 2 else 0


uiFinalAnswerEntry_handler :: Gtk.Builder -> IO ()
uiFinalAnswerEntry_handler builder = do
    Just uiFinalAnswerEntry <- getBuilderObj builder "uiFinalAnswerEntry" Gtk.Entry
    input_Text <- Gtk.entryGetText uiFinalAnswerEntry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = LoadAssociation.getWord $ LoadAssociation.getItem "final" $ GameState.getAssociation gameStateObject
    if user_input == correct_answer then do
        putStrLn "Tacno"
        let bonus_poeni = ((a1_bonus gameStateObject)
                         + (a2_bonus gameStateObject) 
                         + (a3_bonus gameStateObject) 
                         + (a4_bonus gameStateObject)
                         + (a_bonus gameStateObject)
                         + (b1_bonus gameStateObject)
                         + (b2_bonus gameStateObject) 
                         + (b3_bonus gameStateObject) 
                         + (b4_bonus gameStateObject)
                         + (b_bonus gameStateObject)
                         + (c1_bonus gameStateObject)
                         + (c2_bonus gameStateObject) 
                         + (c3_bonus gameStateObject) 
                         + (c4_bonus gameStateObject)
                         + (c_bonus gameStateObject)
                         + (d1_bonus gameStateObject)
                         + (d2_bonus gameStateObject) 
                         + (d3_bonus gameStateObject) 
                         + (d4_bonus gameStateObject)
                         + (d_bonus gameStateObject))

        gameStateObject <- upisiRec "a1" gameStateObject builder
        gameStateObject <- upisiRec "a2" gameStateObject builder
        gameStateObject <- upisiRec "a3" gameStateObject builder
        gameStateObject <- upisiRec "a4" gameStateObject builder
        gameStateObject <- upisiRec "a" gameStateObject builder
        gameStateObject <- upisiRec "b1" gameStateObject builder
        gameStateObject <- upisiRec "b2" gameStateObject builder
        gameStateObject <- upisiRec "b3" gameStateObject builder
        gameStateObject <- upisiRec "b4" gameStateObject builder
        gameStateObject <- upisiRec "b" gameStateObject builder
        gameStateObject <- upisiRec "c1" gameStateObject builder
        gameStateObject <- upisiRec "c2" gameStateObject builder
        gameStateObject <- upisiRec "c3" gameStateObject builder
        gameStateObject <- upisiRec "c4" gameStateObject builder
        gameStateObject <- upisiRec "c" gameStateObject builder
        gameStateObject <- upisiRec "d1" gameStateObject builder
        gameStateObject <- upisiRec "d2" gameStateObject builder
        gameStateObject <- upisiRec "d3" gameStateObject builder
        gameStateObject <- upisiRec "d4" gameStateObject builder
        gameStateObject <- upisiRec "d" gameStateObject builder

        obojiPolje "a1" gameStateObject builder
        obojiPolje "a2" gameStateObject builder
        obojiPolje "a3" gameStateObject builder
        obojiPolje "a4" gameStateObject builder
        obojiPolje "a" gameStateObject builder
        obojiPolje "b1" gameStateObject builder
        obojiPolje "b2" gameStateObject builder
        obojiPolje "b3" gameStateObject builder
        obojiPolje "b4" gameStateObject builder
        obojiPolje "b" gameStateObject builder
        obojiPolje "c1" gameStateObject builder
        obojiPolje "c2" gameStateObject builder
        obojiPolje "c3" gameStateObject builder
        obojiPolje "c4" gameStateObject builder
        obojiPolje "c" gameStateObject builder
        obojiPolje "d1" gameStateObject builder
        obojiPolje "d2" gameStateObject builder
        obojiPolje "d3" gameStateObject builder
        obojiPolje "d4" gameStateObject builder
        obojiPolje "d" gameStateObject builder
        obojiPolje "final" gameStateObject builder


        gameStateObject <- dodajPoene (20 + bonus_poeni) gameStateObject builder
        set uiFinalAnswerEntry [ #editable := False ]
        ukloniFokusSaTrenutnogEntry builder
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        else do
            putStrLn "Netacno"
            gameStateObject <- changePlayerOnMove gameStateObject builder
            Gtk.entrySetText uiFinalAnswerEntry $ T.pack ""
            GameState.saveGameState gameStateObject
            ukloniFokusSaTrenutnogEntry builder

    
    where a1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a1" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a2" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a3" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a4" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          a_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "a" $ GameState.getAssociation gameStateObject) == False then 4 else 0
          b1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "b1" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          b2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "b2" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          b3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "b3" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          b4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "b4" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          b_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "b" $ GameState.getAssociation gameStateObject) == False then 4 else 0
          c1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "c1" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          c2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "c2" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          c3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "c3" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          c4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "c4" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          c_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "c" $ GameState.getAssociation gameStateObject) == False then 4 else 0
          d1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "d1" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          d2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "d2" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          d3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "d3" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          d4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "d4" $ GameState.getAssociation gameStateObject) == False then 2 else 0
          d_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.getItem "d" $ GameState.getAssociation gameStateObject) == False then 4 else 0




entryIdToColumnCell :: String -> String -> String
entryIdToColumnCell entry_id cell
    | cell == "cell1" = (abcd entry_id) ++ "1"
    | cell == "cell2" = (abcd entry_id) ++ "2"
    | cell == "cell3" = (abcd entry_id) ++ "3"
    | cell == "cell4" = (abcd entry_id) ++ "4"
    | otherwise = (abcd entry_id)
    where abcd "uiColumn_A_Entry" = "a"
          abcd "uiColumn_B_Entry" = "b"
          abcd "uiColumn_C_Entry" = "c"
          abcd "uiColumn_D_Entry" = "d"


dodajPoene :: Int -> GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
dodajPoene poeni gameStateObject builder = do
    let trenutni_igrac = GameState.on_move gameStateObject
    let trenutni_poeni = if trenutni_igrac == 1 then GameState.player1_score gameStateObject else GameState.player2_score gameStateObject
    let novi_poeni = trenutni_poeni + poeni
    Just uiPlayerScoreLabel <- getBuilderObj builder (T.pack $ "uiPlayer" ++ (show trenutni_igrac) ++ "ScoreLabel") Gtk.Label
    Gtk.labelSetText uiPlayerScoreLabel $ T.pack $ show novi_poeni
    if trenutni_igrac == 1 then do
        return (gameStateObject{GameState.player1_score = novi_poeni})
    else do
        return (gameStateObject{GameState.player2_score = novi_poeni})

{-uiColumn_B_Entry_handler :: Gtk.Builder ->
uiColumn_B_Entry_handler builder = do
    Just uiColumn_B_Entry <- getBuilderObj builder "uiColumn_B_Entry" Gtk.Entry

uiColumn_C_Entry_handler :: Gtk.Builder ->
uiColumn_C_Entry_handler builder = do
    Just uiColumn_C_Entry <- getBuilderObj builder "uiColumn_C_Entry" Gtk.Entry

uiColumn_D_Entry_handler :: Gtk.Builder ->
uiColumn_D_Entry_handler builder = do
    Just uiColumn_D_Entry <- getBuilderObj builder "uiColumn_D_Entry" Gtk.Entry

uiFinalAnswerEntry_handler :: Gtk.Builder ->
uiFinalAnswerEntry_handler builder = do
    Just uiFinalAnswerEntry <- getBuilderObj builder "uiFinalAnswerEntry" Gtk.Entry-}
