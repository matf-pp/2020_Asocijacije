{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Logic (
    getBuilderObj,
    connectBtnClick,
    connectEntryActivate,
    connectEntryActivate',
    uiButtonPlayOnePlayerClickHandler,
    uiButtonPlayTwoPlayersClickHandler,
    uiButtonSettingsClickHandler,
    uiButtonBackFromSettingsClickHandler,
    poljeHandler,
    kolonaHandler,
    uiFinalAnswerEntry_handler,
    poljeButton,
    kolonaEntry
) where

import Types
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


connectBtnClick :: Maybe Gtk.Button -> IO () -> IO ()
connectBtnClick (Just button) handler = do 
                                        on button #clicked $ do handler
                                        return ()
connectBtnClick Nothing _ = return ()


connectEntryActivate' :: Maybe Gtk.Entry -> IO () -> IO ()
connectEntryActivate' (Just entry) handler = do  
                                            on entry #activate $ do handler
                                            return ()
connectEntryActivate' Nothing _ = do return ()


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
    -- DEBUG
    -- putStrLn $ show $ GameState.getSettings
    
    Just uiPlayer1NameLabel <- getBuilderObj builder "uiPlayer1NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer1NameLabel $ T.pack $ LoadSettings.getItem "player1_name" $ GameState.getSettings
    Just uiPlayer2NameLabel <- getBuilderObj builder "uiPlayer2NameLabel" Gtk.Label
    Gtk.labelSetText uiPlayer2NameLabel $ T.pack $ LoadSettings.getItem "player2_name" $ GameState.getSettings

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


-- Funkcija koja se pokrece klikom polja
poljeHandler :: Polje -> IO ()
poljeHandler polje = do
    -- DEBUG
    -- putStrLn $ polje
    gameStateObject <- GameState.loadGameState
    if ((daLiPoljeNijeOtvoreno gameStateObject) && (daLiIgracNaPotezuNijeOtvaraoPolje gameStateObject)) then do
        gameStateObject <- upisiRecBtn polje gameStateObject
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        gs <- GameState.loadGameState
        -- DEBUG
        -- putStrLn $ show $ gs
        return ()
    else 
        return ()
    where daLiPoljeNijeOtvoreno gameStateObject = (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje polje $ GameState.getAssociation) == False
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


upisiRecBtn :: Polje -> GameState.GameState -> IO (GameState.GameState)
upisiRecBtn polje gameStateObject = do
    let word = LoadAssociation.getWord $ LoadAssociation.uzmiPolje polje $ GameState.getAssociation
    Gtk.buttonSetLabel uiButton $ T.pack word
    return (gameStateObject{GameState.association = LoadAssociation.setItem polje word True $ GameState.getAssociation})
    where (Just uiButton) = poljeButton polje

upisiRecEntry :: Maybe Kolona -> GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
upisiRecEntry kolona gameStateObject builder = do
    let word = LoadAssociation.getWord $ LoadAssociation.uzmiKolonu kolona $ GameState.getAssociation
    Gtk.entrySetText uiEntry $ T.pack word
    return (gameStateObject{GameState.association = LoadAssociation.postaviKolonu kolona word True $ GameState.getAssociation})
    where Just uiEntry = kolonaEntry kolona


upisiRecKolonaPolje :: Polje -> GameState.GameState -> IO (GameState.GameState)
upisiRecKolonaPolje polje gameStateObject = do
    if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje polje $ GameState.getAssociation) == False then do
        gameStateObject <- upisiRecBtn polje gameStateObject
        return (gameStateObject)
        else 
            return (gameStateObject)

upisiRecKolona :: Maybe Kolona -> GameState.GameState -> Gtk.Builder -> IO (GameState.GameState)
upisiRecKolona kolona gameStateObject builder = do
    if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiKolonu kolona $ GameState.getAssociation) == False then do
        gameStateObject <- upisiRecEntry kolona gameStateObject builder
        return (gameStateObject)
        else 
            return (gameStateObject)


obojiPolje :: Polje -> IO ()
obojiPolje polje = do
    styleContextUiButton <- Gtk.widgetGetStyleContext uiButton
    Gtk.styleContextAddClass styleContextUiButton $ T.pack $ "polje-" ++ (LoadSettings.getItem ("player" ++ (show GameState.getMove) ++ "_color") $ GameState.getSettings)
    where Just uiButton = poljeButton polje

obojiKolonu :: Maybe Kolona -> IO ()
obojiKolonu kolona = do
    styleContextUiEntry <- Gtk.widgetGetStyleContext uiEntry
    Gtk.styleContextAddClass styleContextUiEntry $ T.pack $ "polje-" ++ (LoadSettings.getItem ("player" ++ (show GameState.getMove) ++ "_color") $ GameState.getSettings)
    where Just uiEntry = kolonaEntry kolona

kolonaHandler :: Kolona -> Gtk.Builder -> IO ()
kolonaHandler kolona builder = do
    input_Text <- Gtk.entryGetText uiColumn_ABCD_Entry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = LoadAssociation.getWord $ LoadAssociation.uzmiKolonu (Just kolona) $ GameState.getAssociation
    if user_input == correct_answer then do
        let bonus_poeni = ((cell1_bonus gameStateObject)+ (cell2_bonus gameStateObject) + (cell3_bonus gameStateObject) + (cell4_bonus gameStateObject))
        gameStateObject <- upisiRecKolonaPolje (kolona, F1) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (kolona, F2) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (kolona, F3) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (kolona, F4) gameStateObject
        obojiPolje (kolona, F1)
        obojiPolje (kolona, F2)
        obojiPolje (kolona, F3)
        obojiPolje (kolona, F4)
        obojiKolonu (Just kolona)
        gameStateObject <- dodajPoene (10 + bonus_poeni) gameStateObject builder
        set uiColumn_ABCD_Entry [ #editable := False ]
        ukloniFokusSaTrenutnogEntry builder 
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
    else do
        gameStateObject <- changePlayerOnMove gameStateObject builder
        Gtk.entrySetText uiColumn_ABCD_Entry $ T.pack ""
        GameState.saveGameState gameStateObject
        ukloniFokusSaTrenutnogEntry builder
    where Just uiColumn_ABCD_Entry = kolonaEntry $ Just kolona
          cell1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (kolona, F1) $ GameState.getAssociation) == False then 2 else 0
          cell2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (kolona, F2) $ GameState.getAssociation) == False then 2 else 0
          cell3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (kolona, F3) $ GameState.getAssociation) == False then 2 else 0
          cell4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (kolona, F4) $ GameState.getAssociation) == False then 2 else 0


uiFinalAnswerEntry_handler :: Gtk.Builder -> IO ()
uiFinalAnswerEntry_handler builder = do
    Just uiFinalAnswerEntry <- getBuilderObj builder "uiFinalAnswerEntry" Gtk.Entry
    input_Text <- Gtk.entryGetText uiFinalAnswerEntry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = LoadAssociation.getWord $ LoadAssociation.uzmiKolonu Nothing $ GameState.getAssociation
    if user_input == correct_answer then do
        -- DEBUG
        -- putStrLn "Tacno"
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

        gameStateObject <- upisiRecKolonaPolje (A,F1) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (A,F2) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (A,F3) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (A,F4) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (B,F1) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (B,F2) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (B,F3) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (B,F4) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (C,F1) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (C,F2) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (C,F3) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (C,F4) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (D,F1) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (D,F2) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (D,F3) gameStateObject
        gameStateObject <- upisiRecKolonaPolje (D,F4) gameStateObject
        gameStateObject <- upisiRecKolona (Just A) gameStateObject builder
        gameStateObject <- upisiRecKolona (Just B) gameStateObject builder
        gameStateObject <- upisiRecKolona (Just C) gameStateObject builder
        gameStateObject <- upisiRecKolona (Just D) gameStateObject builder

        obojiPolje (A,F1)
        obojiPolje (A,F2)
        obojiPolje (A,F3)
        obojiPolje (A,F4)
        obojiPolje (B,F1)
        obojiPolje (B,F2)
        obojiPolje (B,F3)
        obojiPolje (B,F4)
        obojiPolje (C,F1)
        obojiPolje (C,F2)
        obojiPolje (C,F3)
        obojiPolje (C,F4)
        obojiPolje (D,F1)
        obojiPolje (D,F2)
        obojiPolje (D,F3)
        obojiPolje (D,F4)
        obojiKolonu (Just A)
        obojiKolonu (Just B)
        obojiKolonu (Just C)
        obojiKolonu (Just D)
        obojiKolonu Nothing

        gameStateObject <- dodajPoene (20 + bonus_poeni) gameStateObject builder
        set uiFinalAnswerEntry [ #editable := False ]
        ukloniFokusSaTrenutnogEntry builder
        GameState.saveGameState gameStateObject{GameState.did_on_move_player_open_word = True}
        else do
            -- DEBUG
            putStrLn "Netacno"
            gameStateObject <- changePlayerOnMove gameStateObject builder
            Gtk.entrySetText uiFinalAnswerEntry $ T.pack ""
            GameState.saveGameState gameStateObject
            ukloniFokusSaTrenutnogEntry builder

    
    where a1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (A,F1) $ GameState.getAssociation) == False then 2 else 0
          a2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (A,F2) $ GameState.getAssociation) == False then 2 else 0
          a3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (A,F3) $ GameState.getAssociation) == False then 2 else 0
          a4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (A,F4) $ GameState.getAssociation) == False then 2 else 0
          a_bonus gameStateObject  = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiKolonu (Just A) $ GameState.getAssociation) == False then 4 else 0
          b1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (B,F1) $ GameState.getAssociation) == False then 2 else 0
          b2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (B,F2) $ GameState.getAssociation) == False then 2 else 0
          b3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (B,F3) $ GameState.getAssociation) == False then 2 else 0
          b4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (B,F4) $ GameState.getAssociation) == False then 2 else 0
          b_bonus gameStateObject  = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiKolonu (Just B) $ GameState.getAssociation) == False then 4 else 0
          c1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (C,F1) $ GameState.getAssociation) == False then 2 else 0
          c2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (C,F2) $ GameState.getAssociation) == False then 2 else 0
          c3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (C,F3) $ GameState.getAssociation) == False then 2 else 0
          c4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (C,F4) $ GameState.getAssociation) == False then 2 else 0
          c_bonus gameStateObject  = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiKolonu (Just C) $ GameState.getAssociation) == False then 4 else 0
          d1_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (D,F1) $ GameState.getAssociation) == False then 2 else 0
          d2_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (D,F2) $ GameState.getAssociation) == False then 2 else 0
          d3_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (D,F3) $ GameState.getAssociation) == False then 2 else 0
          d4_bonus gameStateObject = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiPolje (D,F4) $ GameState.getAssociation) == False then 2 else 0
          d_bonus gameStateObject  = if (LoadAssociation.getIsOpened $ LoadAssociation.uzmiKolonu (Just D) $ GameState.getAssociation) == False then 4 else 0


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


poljeButton :: Polje -> Maybe Gtk.Button
poljeButton (A,F1) = a1Button loadUI
poljeButton (A,F2) = a2Button loadUI
poljeButton (A,F3) = a3Button loadUI
poljeButton (A,F4) = a4Button loadUI
poljeButton (B,F1) = b1Button loadUI
poljeButton (B,F2) = b2Button loadUI
poljeButton (B,F3) = b3Button loadUI
poljeButton (B,F4) = b4Button loadUI
poljeButton (C,F1) = c1Button loadUI
poljeButton (C,F2) = c2Button loadUI
poljeButton (C,F3) = c3Button loadUI
poljeButton (C,F4) = c4Button loadUI
poljeButton (D,F1) = d1Button loadUI
poljeButton (D,F2) = d2Button loadUI
poljeButton (D,F3) = d3Button loadUI
poljeButton (D,F4) = d4Button loadUI

kolonaEntry :: Maybe Kolona -> Maybe Gtk.Entry
kolonaEntry (Just A) = aEntry loadUI
kolonaEntry (Just B) = bEntry loadUI
kolonaEntry (Just C) = cEntry loadUI
kolonaEntry (Just D) = dEntry loadUI
kolonaEntry Nothing  = finEntry loadUI