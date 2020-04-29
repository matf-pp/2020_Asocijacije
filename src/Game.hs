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


playButtonHandler :: IO ()
playButtonHandler = do
    Gtk.stackSetVisibleChild uiStack' gameBox'
    makeGameState
    gameStateObject <- loadGameState
    resetUI
    Gtk.labelSetText player1NameLabel' $ T.pack $ LoadSettings.blueName getSettings
    Gtk.labelSetText player2NameLabel' $ T.pack $ LoadSettings.redName  getSettings
    Gtk.labelSetText player1ScoreLabel' $ T.pack $ show $ player1_score gameStateObject 
    Gtk.labelSetText player2ScoreLabel' $ T.pack $ show $ player2_score gameStateObject
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
    gameStateObject <- loadGameState
    if (playerОpenedWord gameStateObject) then do
        gameStateObject <- changePlayerOnMove gameStateObject
        saveGameState gameStateObject
        focusColumn Nothing
    else 
        return ()


fieldHandler :: Field -> IO ()
fieldHandler field = do
    gameStateObject <- loadGameState
    if (fieldClosed && (gamerPlayed gameStateObject)) then do
        writeWordToField field (getWord $ uzmiPolje $ field)
        openAssociation field
        saveGameState gameStateObject{playerОpenedWord = True}
        return ()
    else 
        return ()
    where fieldClosed = not $ getIsOpened $ uzmiPolje field
          gamerPlayed gs = not $ playerОpenedWord gs
        

changePlayerOnMove :: GameState -> IO (GameState)
changePlayerOnMove gameStateObject = do
    styleContextUiPlayer1BoxEventBox <- Gtk.widgetGetStyleContext player1EventBox'
    Gtk.styleContextAddClass styleContextUiPlayer1BoxEventBox $ T.pack "na-potezu"

    styleContextUiPlayer2BoxEventBox <- Gtk.widgetGetStyleContext player2EventBox'
    Gtk.styleContextRemoveClass styleContextUiPlayer2BoxEventBox $ T.pack "na-potezu"

    return (gameStateObject{playerOnMove = sledeciIgrac, playerОpenedWord = False})

    where  Just player1EventBox' | playerOnMove gameStateObject == Plavi = redPlayerEventBox loadUI
                                 | playerOnMove gameStateObject == Crveni = bluePlayerEventBox loadUI
           Just player2EventBox' | playerOnMove gameStateObject == Crveni = redPlayerEventBox loadUI
                                 | playerOnMove gameStateObject == Plavi = bluePlayerEventBox loadUI
           sledeciIgrac | playerOnMove gameStateObject == Crveni = Plavi
                        | playerOnMove gameStateObject == Plavi = Crveni


setFirstPlayerToPlay :: GameState -> IO ()
setFirstPlayerToPlay gameStateObject = do
    if playerOnMove gameStateObject == Plavi then do
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


colorField :: Field -> Igrac -> IO ()
colorField  field player = do 
        styleContextUiButton <- Gtk.widgetGetStyleContext uiButton
        Gtk.styleContextAddClass styleContextUiButton $ colorClass player
        where (Just uiButton) = fieldButton field

colorColumn :: Maybe Column -> Igrac -> IO ()
colorColumn column igrac = do
    styleContextUiEntry <- Gtk.widgetGetStyleContext uiEntry
    Gtk.styleContextAddClass styleContextUiEntry $ colorClass igrac
    where Just uiEntry = columnEntry column


colorClass :: Igrac -> Text
colorClass Plavi = "polje-plava"
colorClass Crveni = "polje-crvena"


columnHandler :: Column -> Gtk.Builder -> IO ()
columnHandler column builder = do
    input_Text <- Gtk.entryGetText entry
    gameStateObject <- loadGameState
    let points = length $ filter (\x -> not $ getIsOpened $ uzmiPolje x) [(column, x) | x <- [F1 .. F4]]
    if (compareStrings (T.unpack input_Text) (getWord $ uzmiKolonu $ Just column)) then do
        traverse_ openAssociation [(column , x) | x <- [F1 .. F4]]
        traverse_ (\x -> openAssociation x >> writeWordToField x (getWord $ uzmiPolje x) >> colorField x (playerOnMove gameStateObject)) [(column , x) | x <- [F1 .. F4]]
        colorColumn (Just column) (playerOnMove gameStateObject)
        writeWordToColumn (Just column) (getWord $ uzmiKolonu $ Just column)
        set entry [ #editable := False ]
        focusColumn Nothing 
        updatePoints gameStateObject
        saveGameState (addPoints points gameStateObject)
        return ()
    else do
        gameStateObject <- changePlayerOnMove gameStateObject
        Gtk.entrySetText entry $ T.pack ""
        saveGameState gameStateObject
        focusColumn Nothing
    where Just entry = columnEntry $ Just column


finalEntryHandler :: IO ()
finalEntryHandler = do
    input_Text <- Gtk.entryGetText uiFinalAnswerEntry
    gameStateObject <- loadGameState
    let correctAnswer = getWord $ uzmiKolonu Nothing
        closedFields = filter (\x -> not $ getIsOpened $ uzmiPolje x) [(x, y) | x <- [A .. D], y <- [F1 .. F4]]
        closedColumns = [(Just x) | x <- [A .. D], (getIsOpened $ uzmiKolonu (Just x)) == False]
    if (compareStrings (T.unpack input_Text) correctAnswer) then do
        traverse_ (\x -> openAssociation x >> colorField x (playerOnMove gameStateObject) >> writeWordToField x (getWord $ uzmiPolje x)) closedFields
        traverse_ (\x -> writeWordToColumn x (getWord $ uzmiKolonu x) >> colorColumn x (playerOnMove gameStateObject)) closedColumns
        colorColumn Nothing (playerOnMove gameStateObject)
        writeWordToColumn Nothing (getWord $ uzmiKolonu Nothing)
        set uiFinalAnswerEntry [ #editable := False ]
        updatePoints gameStateObject
        saveGameState gameStateObject{playerОpenedWord = True}
    else do
        gameStateObject <- changePlayerOnMove gameStateObject
        Gtk.entrySetText uiFinalAnswerEntry $ T.pack ""
        saveGameState gameStateObject
    where Just uiFinalAnswerEntry = finEntry loadUI


updatePoints :: GameState -> IO ()
updatePoints gs = do
    Gtk.labelSetText blueScoreLabel $ T.pack $ show blueScore
    Gtk.labelSetText redScoreLabel $ T.pack $ show redScore
    where blueScore = player1_score gs
          redScore = player2_score gs
          Just blueScoreLabel = bluePlayerScoreLabel loadUI
          Just redScoreLabel = redPlayerScoreLabel loadUI


addPoints :: Int -> GameState -> GameState
addPoints points gs = newGameState (playerOnMove gs) 
            where newGameState Plavi = gs {player1_score = (points + player1_score gs)}
                  newGameState Crveni = gs {player2_score = (points + player2_score gs)}


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


columnEntry :: Maybe Column -> Maybe Gtk.Entry
columnEntry (Just A) = aEntry loadUI
columnEntry (Just B) = bEntry loadUI
columnEntry (Just C) = cEntry loadUI
columnEntry (Just D) = dEntry loadUI
columnEntry Nothing  = finEntry loadUI


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
    traverse_ (\x -> connectBtnClick (fieldButton x) $ fieldHandler x) [(x,y) | x <- [A .. D], y <- [F1 .. F4]]
    traverse_ (\x -> connectEntryActivate (columnEntry (Just x)) $ columnHandler x builder) [A .. D]
    connectEntryActivate (finEntry loadUI) $ finalEntryHandler

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