{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Game (
  createUI
) where

import Types
import qualified LoadSettings
import LoadAssociation
import qualified GameState

import Data.Monoid ((<>))
import qualified Data.Text.IO as T_IO
import qualified Data.Text as T
import Data.Foldable
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
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


connectEntryActivate :: Maybe Gtk.Entry -> IO () -> IO ()
connectEntryActivate (Just entry) handler = do  
                                            on entry #activate $ do handler
                                            return ()
connectEntryActivate Nothing _ = do return ()


focusColumn :: Maybe Column -> IO ()
focusColumn kolona = do
    Gtk.widgetSetCanFocus entry True
    Gtk.widgetGrabFocus entry
    where Just entry = kolonaEntry kolona


ukloniFokusSaTrenutnogEntry :: Gtk.Builder -> IO ()
ukloniFokusSaTrenutnogEntry builder = do
    Just ui_A1_Button <- getBuilderObj builder "ui_A1_Button" Gtk.Button
    Gtk.widgetSetCanFocus ui_A1_Button True
    Gtk.widgetGrabFocus ui_A1_Button

playButtonHandler :: IO ()
playButtonHandler = do
    Gtk.stackSetVisibleChild uiStack' gameBox'
    GameState.makeGameState
    gameStateObject <- GameState.loadGameState
    -- DEBUG
    -- putStrLn $ show $ GameState.getSettings
    Gtk.labelSetText player1NameLabel' $ T.pack $ LoadSettings.blueName GameState.getSettings
    Gtk.labelSetText player2NameLabel' $ T.pack $ LoadSettings.redName  GameState.getSettings
    Gtk.labelSetText player1ScoreLabel' $ T.pack $ show $ GameState.player1_score gameStateObject 
    Gtk.labelSetText player2ScoreLabel' $ T.pack $ show $ GameState.player2_score gameStateObject
    setFirstPlayerToPlay gameStateObject
    focusColumn Nothing
    where Just uiStack' = stack loadUI
          Just gameBox' = gameBox loadUI
          Just player1NameLabel' = bluePlayerNameLabel loadUI
          Just player2NameLabel' = redPlayerNameLabel loadUI
          Just player1ScoreLabel' = bluePlayerScoreLabel loadUI
          Just player2ScoreLabel' = redPlayerScoreLabel loadUI


openSettingsHandler :: IO ()
openSettingsHandler = do
    Gtk.stackSetVisibleChild uiStack uiSettings
    settingsObject <- LoadSettings.readSettingsFile
    -- DEBUG
    -- putStrLn $ show $ settingsObject
    Gtk.entrySetText uiEntry_player1_name $ T.pack $ LoadSettings.blueName settingsObject
    Gtk.entrySetText uiEntry_player1_image $ T.pack $ LoadSettings.blueImage settingsObject
    Gtk.entrySetText uiEntry_player2_name $ T.pack $ LoadSettings.redName settingsObject
    Gtk.entrySetText uiEntry_player2_image $ T.pack $ LoadSettings.redImage settingsObject
    Gtk.comboBoxSetActive combo $ index $ LoadSettings.firstPlay settingsObject
    where Just uiStack = stack loadUI
          Just uiSettings = settingsBox loadUI
          Just uiEntry_player1_name = settingBlueNameEntry loadUI
          Just uiEntry_player1_image = settingBlueImageEntry loadUI
          Just uiEntry_player2_name = settingRedNameEntry loadUI
          Just uiEntry_player2_image = settingRedImageEntry loadUI
          Just combo = settingFirstPlayCombo loadUI
          index x | (x == Plavi) = 0 
                  | (x == Crveni) = 1 


backFromSettingsButtonHandler :: IO ()
backFromSettingsButtonHandler = do
    uiEntry_player1_name_text <- Gtk.getEntryText uiEntry_player1_name
    uiEntry_player1_image_text <- Gtk.getEntryText uiEntry_player1_image
    uiEntry_player2_name_text <- Gtk.getEntryText uiEntry_player2_name
    uiEntry_player2_image_text <- Gtk.getEntryText uiEntry_player2_image
    item <- Gtk.comboBoxGetActive combo
    LoadSettings.writeToSettingsFile $ LoadSettings.Settings (T.unpack uiEntry_player1_name_text) (T.unpack uiEntry_player1_image_text) (T.unpack uiEntry_player2_name_text) (T.unpack uiEntry_player2_image_text) (index item)
    Gtk.stackSetVisibleChild stack' menu
    where Just stack' = stack loadUI
          Just menu = menuBox loadUI
          Just uiEntry_player1_name = settingBlueNameEntry loadUI
          Just uiEntry_player1_image = settingBlueImageEntry loadUI
          Just uiEntry_player2_name = settingRedNameEntry loadUI
          Just uiEntry_player2_image = settingRedImageEntry loadUI
          Just combo = settingFirstPlayCombo loadUI
          index x | (x == 0) = Plavi 
                  | (x == 1) = Crveni


backFromGameHandler :: IO ()
backFromGameHandler = do
    Gtk.stackSetVisibleChild stack' menu
    where Just stack' = stack loadUI
          Just menu = menuBox loadUI


nextButtonHandler :: IO ()
nextButtonHandler = do
    gameStateObject <- GameState.loadGameState
    if (GameState.playerОpenedWord gameStateObject) then do
        gameStateObject <- changePlayerOnMove gameStateObject
        GameState.saveGameState gameStateObject
        focusColumn Nothing
    else 
        return ()


fieldHandler :: Field -> IO ()
fieldHandler polje = do
    -- DEBUG
    -- putStrLn $ polje
    gameStateObject <- GameState.loadGameState
    if (fieldClosed && (gamerPlayed gameStateObject)) then do
        gameStateObject <- upisiRecBtn polje gameStateObject
        GameState.saveGameState gameStateObject{GameState.playerОpenedWord = True}
        return ()
    else 
        return ()
    where fieldClosed = not $ getIsOpened $ uzmiPolje polje GameState.getAssociation
          gamerPlayed gs = not $ GameState.playerОpenedWord gs
        

changePlayerOnMove :: GameState.GameState -> IO (GameState.GameState)
changePlayerOnMove gameStateObject = do
    styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext player1EventBox'
    Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"

    styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext player2EventBox'
    Gtk.styleContextRemoveClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"

    return (gameStateObject{GameState.igracNaPotezu = sledeciIgrac, GameState.playerОpenedWord = False})

    where  Just player1EventBox' | GameState.igracNaPotezu gameStateObject == GameState.Plavi = redPlayerEventBox loadUI
                                 | GameState.igracNaPotezu gameStateObject == GameState.Crveni = bluePlayerEventBox loadUI
           Just player2EventBox' | GameState.igracNaPotezu gameStateObject == GameState.Crveni = redPlayerEventBox loadUI
                                 | GameState.igracNaPotezu gameStateObject == GameState.Plavi = bluePlayerEventBox loadUI
           sledeciIgrac | GameState.igracNaPotezu gameStateObject == GameState.Crveni = GameState.Plavi
                        | GameState.igracNaPotezu gameStateObject == GameState.Plavi = GameState.Crveni


setFirstPlayerToPlay :: GameState.GameState -> IO ()
setFirstPlayerToPlay gameStateObject = do
    if GameState.igracNaPotezu gameStateObject == GameState.Plavi then do
        styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext player1EventBox'
        Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"
    else do
        styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext player2EventBox'
        Gtk.styleContextAddClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"
    where Just player1EventBox' = bluePlayerEventBox loadUI
          Just player2EventBox' = redPlayerEventBox loadUI


upisiRecBtn :: Field -> GameState.GameState -> IO (GameState.GameState)
upisiRecBtn polje gameStateObject = do
    let word = getWord $ uzmiPolje polje $ GameState.getAssociation
    Gtk.buttonSetLabel uiButton $ T.pack word
    return (gameStateObject{GameState.association = setItem polje word True $ GameState.getAssociation})
    where (Just uiButton) = poljeButton polje

upisiRecEntry :: Maybe Column -> GameState.GameState -> IO (GameState.GameState)
upisiRecEntry kolona gameStateObject = do
    let word = getWord $ uzmiKolonu kolona $ GameState.getAssociation
    Gtk.entrySetText uiEntry $ T.pack word
    return (gameStateObject{GameState.association = postaviKolonu kolona word True $ GameState.getAssociation})
    where Just uiEntry = kolonaEntry kolona


openField :: Field -> GameState.GameState -> IO (GameState.GameState)
openField polje gameStateObject = do
    if (getIsOpened $ uzmiPolje polje $ GameState.getAssociation) == False then do
        gameStateObject <- upisiRecBtn polje gameStateObject
        return (gameStateObject)
    else 
        return (gameStateObject)

upisiRecKolona :: Maybe Column -> GameState.GameState -> IO (GameState.GameState)
upisiRecKolona kolona gameStateObject = do
    if (getIsOpened $ uzmiKolonu kolona $ GameState.getAssociation) == False then do
        gameStateObject <- upisiRecEntry kolona gameStateObject
        return (gameStateObject)
    else 
        return (gameStateObject)


obojiPolje :: Field -> GameState.Igrac -> IO ()
obojiPolje polje igrac = do
    putStrLn $ show polje 
    styleContextUiButton <- Gtk.widgetGetStyleContext uiButton
    Gtk.styleContextAddClass styleContextUiButton $ colorClass igrac
    where Just uiButton = poljeButton polje
          

obojiKolonu :: Maybe Column -> GameState.Igrac -> IO ()
obojiKolonu kolona igrac = do
    styleContextUiEntry <- Gtk.widgetGetStyleContext uiEntry
    Gtk.styleContextAddClass styleContextUiEntry $ colorClass igrac
    where Just uiEntry = kolonaEntry kolona


colorClass :: GameState.Igrac -> Text
colorClass GameState.Plavi = "polje-plava"
colorClass GameState.Crveni = "polje-crvena"


columnHandler :: Column -> Gtk.Builder -> IO ()
columnHandler kolona builder = do
    input_Text <- Gtk.entryGetText entry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = getWord $ uzmiKolonu (Just kolona)  GameState.getAssociation
    if user_input == correct_answer then do
        -- DEBUG 
        putStrLn "Tacno"
        gameStateObject <- openField (kolona, F1) gameStateObject
        gameStateObject <- openField (kolona, F2) gameStateObject
        gameStateObject <- openField (kolona, F3) gameStateObject
        gameStateObject <- openField (kolona, F4) gameStateObject
        traverse_ (\x -> obojiPolje (kolona, x) $ GameState.igracNaPotezu gameStateObject) [F1 .. F4]
        obojiKolonu (Just kolona) (GameState.igracNaPotezu gameStateObject)
        gameStateObject <- addPoints (10 + 2*(length [ x | x <- [F1 .. F4], (getIsOpened $ uzmiPolje (kolona, x) $ GameState.getAssociation) == False ] )) (GameState.igracNaPotezu gameStateObject) gameStateObject
        set entry [ #editable := False ]
        focusColumn Nothing 
        GameState.saveGameState gameStateObject
    else do
        -- DEBUG
        putStrLn "Netacno"
        gameStateObject <- changePlayerOnMove gameStateObject
        Gtk.entrySetText entry $ T.pack ""
        GameState.saveGameState gameStateObject
        focusColumn Nothing
    where Just entry = kolonaEntry $ Just kolona


uiFinalAnswerEntry_handler :: Gtk.Builder -> IO ()
uiFinalAnswerEntry_handler builder = do
    Just uiFinalAnswerEntry <- getBuilderObj builder "uiFinalAnswerEntry" Gtk.Entry
    input_Text <- Gtk.entryGetText uiFinalAnswerEntry
    let user_input = T.unpack input_Text
    gameStateObject <- GameState.loadGameState
    let correct_answer = getWord $ uzmiKolonu Nothing $ GameState.getAssociation
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

        gameStateObject <- openField (A,F1) gameStateObject
        gameStateObject <- openField (A,F2) gameStateObject
        gameStateObject <- openField (A,F3) gameStateObject
        gameStateObject <- openField (A,F4) gameStateObject
        gameStateObject <- openField (B,F1) gameStateObject
        gameStateObject <- openField (B,F2) gameStateObject
        gameStateObject <- openField (B,F3) gameStateObject
        gameStateObject <- openField (B,F4) gameStateObject
        gameStateObject <- openField (C,F1) gameStateObject
        gameStateObject <- openField (C,F2) gameStateObject
        gameStateObject <- openField (C,F3) gameStateObject
        gameStateObject <- openField (C,F4) gameStateObject
        gameStateObject <- openField (D,F1) gameStateObject
        gameStateObject <- openField (D,F2) gameStateObject
        gameStateObject <- openField (D,F3) gameStateObject
        gameStateObject <- openField (D,F4) gameStateObject
        gameStateObject <- upisiRecKolona (Just A) gameStateObject
        gameStateObject <- upisiRecKolona (Just B) gameStateObject
        gameStateObject <- upisiRecKolona (Just C) gameStateObject
        gameStateObject <- upisiRecKolona (Just D) gameStateObject

        obojiPolje (A,F1) $ GameState.igracNaPotezu gameStateObject 
        obojiPolje (A,F2) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (A,F3) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (A,F4) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (B,F1) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (B,F2) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (B,F3) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (B,F4) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (C,F1) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (C,F2) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (C,F3) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (C,F4) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (D,F1) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (D,F2) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (D,F3) $ GameState.igracNaPotezu gameStateObject
        obojiPolje (D,F4) $ GameState.igracNaPotezu gameStateObject
        obojiKolonu (Just A) (GameState.igracNaPotezu gameStateObject) 
        obojiKolonu (Just B) (GameState.igracNaPotezu gameStateObject)
        obojiKolonu (Just C) (GameState.igracNaPotezu gameStateObject)
        obojiKolonu (Just D) (GameState.igracNaPotezu gameStateObject)
        obojiKolonu Nothing (GameState.igracNaPotezu gameStateObject)

        gameStateObject <- addPoints (20 + bonus_poeni) (GameState.igracNaPotezu gameStateObject) gameStateObject
        set uiFinalAnswerEntry [ #editable := False ]
        ukloniFokusSaTrenutnogEntry builder
        GameState.saveGameState gameStateObject{GameState.playerОpenedWord = True}
        else do
            -- DEBUG
            putStrLn "Netacno"
            gameStateObject <- changePlayerOnMove gameStateObject
            Gtk.entrySetText uiFinalAnswerEntry $ T.pack ""
            GameState.saveGameState gameStateObject
            ukloniFokusSaTrenutnogEntry builder

    
    where a1_bonus gameStateObject = if (getIsOpened $ uzmiPolje (A,F1) $ GameState.getAssociation) == False then 2 else 0
          a2_bonus gameStateObject = if (getIsOpened $ uzmiPolje (A,F2) $ GameState.getAssociation) == False then 2 else 0
          a3_bonus gameStateObject = if (getIsOpened $ uzmiPolje (A,F3) $ GameState.getAssociation) == False then 2 else 0
          a4_bonus gameStateObject = if (getIsOpened $ uzmiPolje (A,F4) $ GameState.getAssociation) == False then 2 else 0
          a_bonus gameStateObject  = if (getIsOpened $ uzmiKolonu (Just A) $ GameState.getAssociation) == False then 4 else 0
          b1_bonus gameStateObject = if (getIsOpened $ uzmiPolje (B,F1) $ GameState.getAssociation) == False then 2 else 0
          b2_bonus gameStateObject = if (getIsOpened $ uzmiPolje (B,F2) $ GameState.getAssociation) == False then 2 else 0
          b3_bonus gameStateObject = if (getIsOpened $ uzmiPolje (B,F3) $ GameState.getAssociation) == False then 2 else 0
          b4_bonus gameStateObject = if (getIsOpened $ uzmiPolje (B,F4) $ GameState.getAssociation) == False then 2 else 0
          b_bonus gameStateObject  = if (getIsOpened $ uzmiKolonu (Just B) $ GameState.getAssociation) == False then 4 else 0
          c1_bonus gameStateObject = if (getIsOpened $ uzmiPolje (C,F1) $ GameState.getAssociation) == False then 2 else 0
          c2_bonus gameStateObject = if (getIsOpened $ uzmiPolje (C,F2) $ GameState.getAssociation) == False then 2 else 0
          c3_bonus gameStateObject = if (getIsOpened $ uzmiPolje (C,F3) $ GameState.getAssociation) == False then 2 else 0
          c4_bonus gameStateObject = if (getIsOpened $ uzmiPolje (C,F4) $ GameState.getAssociation) == False then 2 else 0
          c_bonus gameStateObject  = if (getIsOpened $ uzmiKolonu (Just C) $ GameState.getAssociation) == False then 4 else 0
          d1_bonus gameStateObject = if (getIsOpened $ uzmiPolje (D,F1) $ GameState.getAssociation) == False then 2 else 0
          d2_bonus gameStateObject = if (getIsOpened $ uzmiPolje (D,F2) $ GameState.getAssociation) == False then 2 else 0
          d3_bonus gameStateObject = if (getIsOpened $ uzmiPolje (D,F3) $ GameState.getAssociation) == False then 2 else 0
          d4_bonus gameStateObject = if (getIsOpened $ uzmiPolje (D,F4) $ GameState.getAssociation) == False then 2 else 0
          d_bonus gameStateObject  = if (getIsOpened $ uzmiKolonu (Just D) $ GameState.getAssociation) == False then 4 else 0


addPoints :: Int -> Igrac -> GameState.GameState -> IO (GameState.GameState)
addPoints points Plavi gameStateObject = do
    Gtk.labelSetText scoreLabel $ T.pack $ show newScore
    return (gameStateObject{GameState.player1_score = newScore})
    where newScore = (GameState.player1_score gameStateObject) + points
          Just scoreLabel = bluePlayerScoreLabel loadUI
addPoints points Crveni gameStateObject = do
    Gtk.labelSetText scoreLabel $ T.pack $ show newScore
    return (gameStateObject{GameState.player2_score = newScore})
    where newScore = (GameState.player2_score gameStateObject) + points
          Just scoreLabel = redPlayerScoreLabel loadUI



poljeButton :: Field -> Maybe Gtk.Button
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

kolonaEntry :: Maybe Column -> Maybe Gtk.Entry
kolonaEntry (Just A) = aEntry loadUI
kolonaEntry (Just B) = bEntry loadUI
kolonaEntry (Just C) = cEntry loadUI
kolonaEntry (Just D) = dEntry loadUI
kolonaEntry Nothing  = finEntry loadUI


loadCSS :: IO ()
loadCSS = do
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


createUI :: Maybe [T.Text] -> IO ()
createUI args = do
    Gtk.init args
    builder <- Gtk.builderNewFromResource "/asocijacije/resources/ui.glade"
    loadCSS

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
    startGameButton' <- getBuilderObj builder (T.pack "uiButtonPlay") Gtk.Button
    settingsButton' <- getBuilderObj builder (T.pack "uiButtonSettings") Gtk.Button
    backButton' <- getBuilderObj builder (T.pack "uiButtonBackFromSettings") Gtk.Button
    nextButton' <- getBuilderObj builder (T.pack "gameNextButton") Gtk.Button
    gameExitButton' <- getBuilderObj builder (T.pack "gameExitButton") Gtk.Button
    quitButton' <- getBuilderObj builder (T.pack "uiButtonQuit") Gtk.Button
    stack' <- getBuilderObj builder "uiStack" Gtk.Stack
    menuBox' <- getBuilderObj builder "uiMainMenu" Gtk.Box
    gameBox' <- getBuilderObj builder "uiTwoPlayersGameEventBox" Gtk.EventBox
    settingsBox' <- getBuilderObj builder "uiSettings" Gtk.Box
    bluePlayerNameLabel' <- getBuilderObj builder "uiPlayer1NameLabel" Gtk.Label
    redPlayerNameLabel' <- getBuilderObj builder "uiPlayer2NameLabel" Gtk.Label
    bluePlayerScoreLabel' <- getBuilderObj builder "uiPlayer1ScoreLabel" Gtk.Label
    redPlayerScoreLabel' <- getBuilderObj builder "uiPlayer2ScoreLabel" Gtk.Label
    bluePlayerEventBox' <- getBuilderObj builder "uiPlayer1BoxEventBox" Gtk.EventBox
    redPlayerEventBox' <- getBuilderObj builder "uiPlayer2BoxEventBox" Gtk.EventBox
    settingBlueNameEntry' <- getBuilderObj builder "uiEntry_player1_name" Gtk.Entry
    settingBlueImageEntry' <- getBuilderObj builder "uiEntry_player1_image" Gtk.Entry
    settingRedNameEntry' <- getBuilderObj builder "uiEntry_player2_name" Gtk.Entry
    settingRedImageEntry' <- getBuilderObj builder "uiEntry_player2_image" Gtk.Entry
    settingFirstPlayCombo' <- getBuilderObj builder "uiComboBoxText_first_play" Gtk.ComboBoxText

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
                  gameExitButton = gameExitButton',
                  quitButton = quitButton',
                  stack = stack',
                  gameBox = gameBox',
                  menuBox = menuBox',
                  settingsBox = settingsBox',
                  bluePlayerNameLabel = bluePlayerNameLabel',
                  redPlayerNameLabel = redPlayerNameLabel',
                  bluePlayerScoreLabel = bluePlayerScoreLabel',
                  redPlayerScoreLabel = redPlayerScoreLabel',
                  bluePlayerEventBox = bluePlayerEventBox',
                  redPlayerEventBox = redPlayerEventBox',
                  settingBlueNameEntry = settingBlueNameEntry',
                  settingBlueImageEntry = settingBlueImageEntry',
                  settingRedNameEntry = settingRedNameEntry',
                  settingRedImageEntry = settingRedImageEntry',
                  settingFirstPlayCombo = settingFirstPlayCombo'
                }

    connectBtnClick (startGameButton loadUI) playButtonHandler
    connectBtnClick (settingsButton loadUI) openSettingsHandler
    connectBtnClick (backButton loadUI) backFromSettingsButtonHandler
    connectBtnClick (quitButton loadUI) Gtk.mainQuit
    connectBtnClick (gameExitButton loadUI) backFromGameHandler
    connectBtnClick (nextButton loadUI) nextButtonHandler
    traverse_ (\x -> connectBtnClick (poljeButton x) $ fieldHandler x) [(x,y) | x <- [A .. D], y <- [F1 .. F4]]
    traverse_ (\x -> connectEntryActivate (kolonaEntry (Just x)) $ columnHandler x builder) [A .. D]
    connectEntryActivate (finEntry loadUI) $ uiFinalAnswerEntry_handler builder

    Gtk.main
