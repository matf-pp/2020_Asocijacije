{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Game (
  createUI
) where

import Types
import qualified LoadSettings
import LoadAssociation
import GameState
import Strings
import Data.Monoid ((<>))
import qualified Data.Text.IO as T_IO
import qualified Data.Text as T
import Data.Foldable
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Control.Monad
import Data.IORef


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
focusColumn column = do
    Gtk.widgetSetCanFocus entry True
    Gtk.widgetGrabFocus entry
    where Just entry = columnEntry column


playButtonHandler :: IORef GameState -> IO ()
playButtonHandler gsRef = do
    Gtk.stackSetVisibleChild uiStack' gameBox'
    makeGameState gsRef
    resetUI
    gs <- gameState gsRef
    Gtk.labelSetText player1NameLabel' $ T.pack $ LoadSettings.blueName $ settings gs
    Gtk.labelSetText player2NameLabel' $ T.pack $ LoadSettings.redName  $ settings gs
    Gtk.labelSetText player1ScoreLabel' $ T.pack "0" 
    Gtk.labelSetText player2ScoreLabel' $ T.pack "0"
    set entryA [ #editable := True ]
    set entryB [ #editable := True ]
    set entryC [ #editable := True ]
    set entryD [ #editable := True ]
    set entryF [ #editable := True ]
    setFirstPlayerToPlay $ playerOnMove gs
    focusColumn Nothing
    where Just uiStack' = stack loadUI
          Just gameBox' = gameBox loadUI
          Just player1NameLabel' = bluePlayerNameLabel loadUI
          Just player2NameLabel' = redPlayerNameLabel loadUI
          Just player1ScoreLabel' = bluePlayerScoreLabel loadUI
          Just player2ScoreLabel' = redPlayerScoreLabel loadUI
          Just entryA = aEntry loadUI
          Just entryB = bEntry loadUI
          Just entryC = cEntry loadUI
          Just entryD = dEntry loadUI
          Just entryF = finEntry loadUI


openSettingsHandler :: IO ()
openSettingsHandler = do
    Gtk.stackSetVisibleChild uiStack uiSettings
    settingsObject <- LoadSettings.readSettingsFile
    Gtk.entrySetText uiEntry_player1_name $ T.pack $ LoadSettings.blueName settingsObject
    Gtk.entrySetText uiEntry_player2_name $ T.pack $ LoadSettings.redName settingsObject
    Gtk.comboBoxSetActive combo $ index $ LoadSettings.firstPlay settingsObject
    where Just uiStack = stack loadUI
          Just uiSettings = settingsBox loadUI
          Just uiEntry_player1_name = settingBlueNameEntry loadUI
          Just uiEntry_player2_name = settingRedNameEntry loadUI
          Just combo = settingFirstPlayCombo loadUI
          index x | (x == Plavi) = 0 
                  | (x == Crveni) = 1 


backFromSettingsButtonHandler :: IO ()
backFromSettingsButtonHandler = do
    uiEntry_player1_name_text <- Gtk.getEntryText uiEntry_player1_name
    uiEntry_player2_name_text <- Gtk.getEntryText uiEntry_player2_name
    item <- Gtk.comboBoxGetActive combo
    LoadSettings.writeToSettingsFile $ LoadSettings.Settings (T.unpack uiEntry_player1_name_text) (T.unpack uiEntry_player2_name_text) (index item)
    Gtk.stackSetVisibleChild stack' menu
    where Just stack' = stack loadUI
          Just menu = menuBox loadUI
          Just uiEntry_player1_name = settingBlueNameEntry loadUI
          Just uiEntry_player2_name = settingRedNameEntry loadUI
          Just combo = settingFirstPlayCombo loadUI
          index x | (x == 0) = Plavi 
                  | (x == 1) = Crveni


backFromGameHandler :: IO ()
backFromGameHandler = do
    Gtk.stackSetVisibleChild stack' menu
    where Just stack' = stack loadUI
          Just menu = menuBox loadUI


nextButtonHandler :: IORef GameState -> IO ()
nextButtonHandler gsRef = do
    gs <- gameState gsRef
    if (playerОpenedWord gs) then do
        changePlayerOnMove gsRef
        focusColumn Nothing
    else 
        return ()


fieldHandler :: Field -> IORef GameState -> IO ()
fieldHandler field gsRef = do
    gs <- gameState gsRef
    if ((not $ getIsOpened $ takeField field gs) && (gamerPlayed gs)) then do
        writeWordToField field (getWord $ takeField field gs)
        saveGameState gsRef $ (openField field) $ setPOW gs
    else 
        return ()
    where gamerPlayed gs' = not $ playerОpenedWord gs'


setPOW :: GameState -> GameState
setPOW gs = gs {playerОpenedWord = True}


changePlayerOnMove :: IORef GameState -> IO ()
changePlayerOnMove gsRef = do
    gs <- gameState gsRef
    styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext player1EventBox'
    Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"
    styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext player2EventBox'
    Gtk.styleContextRemoveClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"
    saveGameState gsRef gs{playerOnMove = (sledeciIgrac (playerOnMove gs)), playerОpenedWord = False}
    where  Just player1EventBox' | playerOnMove (gameState' gsRef) == Plavi = redPlayerEventBox loadUI
                                 | playerOnMove (gameState' gsRef) == Crveni = bluePlayerEventBox loadUI
           Just player2EventBox' | playerOnMove (gameState' gsRef) == Crveni = redPlayerEventBox loadUI
                                 | playerOnMove (gameState' gsRef) == Plavi = bluePlayerEventBox loadUI
           sledeciIgrac Crveni = Plavi
           sledeciIgrac Plavi = Crveni


setFirstPlayerToPlay :: Player -> IO ()
setFirstPlayerToPlay player = do
    if player == Plavi then do
        styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext player1EventBox'
        Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"
    else do
        styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext player2EventBox'
        Gtk.styleContextAddClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"
    where Just player1EventBox' = bluePlayerEventBox loadUI
          Just player2EventBox' = redPlayerEventBox loadUI


writeWordToField :: Field -> String -> IO ()
writeWordToField polje string = do
    Gtk.buttonSetLabel uiButton $ T.pack string
    where (Just uiButton) = fieldButton polje


writeWordToColumn :: Maybe Column -> String -> IO ()
writeWordToColumn column string = do
    Gtk.entrySetText uiEntry $ T.pack string
    where Just uiEntry = columnEntry column


colorField :: Field -> Player -> IO ()
colorField  field player = do 
        styleContextUiButton <- Gtk.widgetGetStyleContext uiButton
        Gtk.styleContextAddClass styleContextUiButton $ colorClass player
        where (Just uiButton) = fieldButton field


colorColumn :: Maybe Column -> Player -> IO ()
colorColumn column igrac = do
    styleContextUiEntry <- Gtk.widgetGetStyleContext uiEntry
    Gtk.styleContextAddClass styleContextUiEntry $ colorClass igrac
    where Just uiEntry = columnEntry column


colorClass :: Player -> Text
colorClass Plavi = "polje-plava"
colorClass Crveni = "polje-crvena"


columnHandler :: Column -> IORef GameState -> IO ()
columnHandler column gsRef = do
    gs <- gameState gsRef
    input_Text <- Gtk.entryGetText entry
    let points = length $ filter (\x -> not $ getIsOpened $ takeField x gs) fields
        player = playerOnMove gs
    if (compareStrings (T.unpack input_Text) (getWord $ takeColumn (Just column) gs)) then do
        putStrLn $ show points
        saveGameState gsRef $ openColumn (Just column) $ addPoints points player $ openFields gs fields
        traverse_ (\x -> writeWordToField x (getWord $ takeField x gs) >> colorField x player) fields
        colorColumn (Just column) player
        writeWordToColumn (Just column) (getWord $ takeColumn (Just column) gs)
        set entry [ #editable := False ]
        focusColumn Nothing
        updatePoints gsRef
    else do
        changePlayerOnMove gsRef
        Gtk.entrySetText entry $ T.pack ""
        focusColumn Nothing
    where Just entry = columnEntry $ Just column
          fields = [(column , x) | x <- [F1 .. F4]]


openFields :: GameState -> [Field] -> GameState
openFields s (x:xs) = openField x $ openFields s xs 
openFields s []     = s
 

finalEntryHandler :: IORef GameState -> IO ()
finalEntryHandler gsRef = do
    input_Text <- Gtk.entryGetText uiFinalAnswerEntry
    gs <- gameState gsRef
    let player = playerOnMove gs
        correctAnswer = getWord $ takeColumn Nothing gs
        closedFields = filter (\x -> not $ getIsOpened $ takeField x gs) [(x, y) | x <- [A .. D], y <- [F1 .. F4]]
        closedColumns = [(Just x) | x <- [A .. D], (getIsOpened $ takeColumn (Just x) gs) == False]
    if (compareStrings (T.unpack input_Text) correctAnswer) then do
        traverse_ (\x -> colorField x player >> writeWordToField x (getWord $ takeField x gs)) closedFields
        traverse_ (\x -> writeWordToColumn x (getWord $ takeColumn x gs) >> colorColumn x player) closedColumns
        colorColumn Nothing player
        writeWordToColumn Nothing (getWord $ takeColumn Nothing gs)
        set uiFinalAnswerEntry [ #editable := False ]
        saveGameState gsRef $ addPoints (2 * length closedFields + 5 * length closedColumns + 10) player gs
        updatePoints gsRef
    else do
        Gtk.entrySetText uiFinalAnswerEntry $ T.pack ""
        changePlayerOnMove gsRef
        return ()
    where Just uiFinalAnswerEntry = finEntry loadUI


updatePoints :: IORef GameState -> IO ()
updatePoints gsRef = do
    gs <- gameState gsRef
    Gtk.labelSetText blueScoreLabel $ T.pack $ show $ player1_score gs
    Gtk.labelSetText redScoreLabel $ T.pack $ show $ player2_score gs
    where Just blueScoreLabel = bluePlayerScoreLabel loadUI
          Just redScoreLabel = redPlayerScoreLabel loadUI


addPoints :: Int -> Player -> GameState -> GameState
addPoints points player gs = newGameState player 
            where newGameState Plavi = gs {player1_score = (points + player1_score gs)}
                  newGameState Crveni = gs {player2_score = (points + player2_score gs)}


loadCSS :: IO ()
loadCSS = do
    maybeScreen <- Gdk.screenGetDefault
    provider <- Gtk.cssProviderNew

    case (maybeScreen) of
        (Just screen) -> do
            Gtk.cssProviderLoadFromResource provider "/asocijacije/resources/style.css"
            Gtk.styleContextAddProviderForScreen
                screen
                provider
                (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
    return ()


createUI :: IORef GameState -> Maybe [T.Text] -> IO ()
createUI gs args = do
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
    settingRedNameEntry' <- getBuilderObj builder "uiEntry_player2_name" Gtk.Entry
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
                  settingRedNameEntry = settingRedNameEntry',
                  settingFirstPlayCombo = settingFirstPlayCombo'
                }

    connectBtnClick (startGameButton loadUI) (playButtonHandler gs)
    connectBtnClick (settingsButton loadUI) openSettingsHandler
    connectBtnClick (backButton loadUI) backFromSettingsButtonHandler
    connectBtnClick (quitButton loadUI) Gtk.mainQuit
    connectBtnClick (gameExitButton loadUI) backFromGameHandler
    connectBtnClick (nextButton loadUI) (nextButtonHandler gs)
    traverse_ (\x -> connectBtnClick (fieldButton x) $ fieldHandler x gs) [(x,y) | x <- [A .. D], y <- [F1 .. F4]]
    traverse_ (\x -> connectEntryActivate (columnEntry (Just x)) $ columnHandler x gs) [A .. D]
    connectEntryActivate (finEntry loadUI) $ (finalEntryHandler gs)

    Gtk.main


resetUI :: IO ()
resetUI = do
    traverse_ (\x -> removeButtonStyle (fieldButton $ fst x) >> writeWordToField (fst x) (snd x)) $ zip [(x,y) | x <- [A .. D], y <- [F1 .. F4]] [ x ++ y | x <- ["A", "Б", "В", "Г"], y <- ["1", "2", "3", "4"]]
    traverse_ (\x -> removeEntryStyle (columnEntry x) >> writeWordToColumn x "") [Just A, Just B, Just C, Just D, Nothing]


removeButtonStyle :: Maybe Gtk.Button -> IO ()
removeButtonStyle (Just button) = do
        context <- Gtk.widgetGetStyleContext button
        Gtk.styleContextRemoveClass context $ colorClass Plavi
        Gtk.styleContextRemoveClass context $ colorClass Crveni
removeButtonStyle Nothing = return ()


removeEntryStyle :: Maybe Gtk.Entry -> IO ()
removeEntryStyle (Just entry) = do
        context <- Gtk.widgetGetStyleContext entry
        Gtk.styleContextRemoveClass context $ colorClass Plavi
        Gtk.styleContextRemoveClass context $ colorClass Crveni
removeEntryStyle Nothing = return ()


openField :: Field -> GameState -> GameState
openField (A,F1) gs = gs { assoc = (assoc gs){ a1_private = (fst (a1_private (assoc gs)), True)} }
openField (A,F2) gs = gs { assoc = (assoc gs){ a2_private = (fst (a2_private (assoc gs)), True)} }
openField (A,F3) gs = gs { assoc = (assoc gs){ a3_private = (fst (a3_private (assoc gs)), True)} }
openField (A,F4) gs = gs { assoc = (assoc gs){ a4_private = (fst (a4_private (assoc gs)), True)} }
openField (B,F1) gs = gs { assoc = (assoc gs){ b1_private = (fst (b1_private (assoc gs)), True)} }
openField (B,F2) gs = gs { assoc = (assoc gs){ b2_private = (fst (b2_private (assoc gs)), True)} }
openField (B,F3) gs = gs { assoc = (assoc gs){ b3_private = (fst (b3_private (assoc gs)), True)} }
openField (B,F4) gs = gs { assoc = (assoc gs){ b4_private = (fst (b4_private (assoc gs)), True)} }
openField (C,F1) gs = gs { assoc = (assoc gs){ c1_private = (fst (c1_private (assoc gs)), True)} }
openField (C,F2) gs = gs { assoc = (assoc gs){ c2_private = (fst (c2_private (assoc gs)), True)} }
openField (C,F3) gs = gs { assoc = (assoc gs){ c3_private = (fst (c3_private (assoc gs)), True)} }
openField (C,F4) gs = gs { assoc = (assoc gs){ c4_private = (fst (c4_private (assoc gs)), True)} }
openField (D,F1) gs = gs { assoc = (assoc gs){ d1_private = (fst (d1_private (assoc gs)), True)} }
openField (D,F2) gs = gs { assoc = (assoc gs){ d2_private = (fst (d2_private (assoc gs)), True)} }
openField (D,F3) gs = gs { assoc = (assoc gs){ d3_private = (fst (d3_private (assoc gs)), True)} }
openField (D,F4) gs = gs { assoc = (assoc gs){ d4_private = (fst (d4_private (assoc gs)), True)} }


takeField :: Field -> GameState -> PairWordIsOpened 
takeField (A,F1) gs = (a1_private . assoc) gs 
takeField (A,F2) gs = (a2_private . assoc) gs 
takeField (A,F3) gs = (a3_private . assoc) gs 
takeField (A,F4) gs = (a4_private . assoc) gs 
takeField (B,F1) gs = (b1_private . assoc) gs 
takeField (B,F2) gs = (b2_private . assoc) gs 
takeField (B,F3) gs = (b3_private . assoc) gs 
takeField (B,F4) gs = (b4_private . assoc) gs 
takeField (C,F1) gs = (c1_private . assoc) gs 
takeField (C,F2) gs = (c2_private . assoc) gs 
takeField (C,F3) gs = (c3_private . assoc) gs 
takeField (C,F4) gs = (c4_private . assoc) gs 
takeField (D,F1) gs = (d1_private . assoc) gs 
takeField (D,F2) gs = (d2_private . assoc) gs 
takeField (D,F3) gs = (d3_private . assoc) gs 
takeField (D,F4) gs = (d4_private . assoc) gs


fieldButton :: Field -> Maybe Gtk.Button
fieldButton (A,F1) = a1Button loadUI
fieldButton (A,F2) = a2Button loadUI
fieldButton (A,F3) = a3Button loadUI
fieldButton (A,F4) = a4Button loadUI
fieldButton (B,F1) = b1Button loadUI
fieldButton (B,F2) = b2Button loadUI
fieldButton (B,F3) = b3Button loadUI
fieldButton (B,F4) = b4Button loadUI
fieldButton (C,F1) = c1Button loadUI
fieldButton (C,F2) = c2Button loadUI
fieldButton (C,F3) = c3Button loadUI
fieldButton (C,F4) = c4Button loadUI
fieldButton (D,F1) = d1Button loadUI
fieldButton (D,F2) = d2Button loadUI
fieldButton (D,F3) = d3Button loadUI
fieldButton (D,F4) = d4Button loadUI


openColumn :: Maybe Column -> GameState -> GameState
openColumn (Just A) gs = gs { assoc = (assoc gs){ a_private = (fst (a_private (assoc gs)), True)} }
openColumn (Just B) gs = gs { assoc = (assoc gs){ b_private = (fst (b_private (assoc gs)), True)} }
openColumn (Just C) gs = gs { assoc = (assoc gs){ c_private = (fst (c_private (assoc gs)), True)} }
openColumn (Just D) gs = gs { assoc = (assoc gs){ d_private = (fst (d_private (assoc gs)), True)} }
openColumn Nothing  gs = gs { assoc = (assoc gs){ final_private = (fst (final_private (assoc gs)), True)} }


takeColumn :: Maybe Column -> GameState -> PairWordIsOpened
takeColumn (Just A) gs = a_private $ assoc gs
takeColumn (Just B) gs = b_private $ assoc gs
takeColumn (Just C) gs = c_private $ assoc gs
takeColumn (Just D) gs = d_private $ assoc gs
takeColumn Nothing  gs = final_private $ assoc gs


columnEntry :: Maybe Column -> Maybe Gtk.Entry
columnEntry (Just A) = aEntry loadUI
columnEntry (Just B) = bEntry loadUI
columnEntry (Just C) = cEntry loadUI
columnEntry (Just D) = dEntry loadUI
columnEntry Nothing  = finEntry loadUI