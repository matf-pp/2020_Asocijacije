{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, DeriveDataTypeable #-}
module Logic 
(
printQuit,
getBuilderObj,
connectBtnClick,
uiButtonPlayTwoPlayersClickHandler,
uiButtonSettingsClickHandler,
uiButtonBackFromSettingsClickHandler
) where
    
import qualified LoadSettings

import Data.Monoid ((<>))
import qualified Data.Text.IO as T_IO
import qualified Data.Text as T
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import Data.GI.Base

printQuit :: Text -> IO ()
printQuit t = do
    T_IO.putStrLn $ "Quitting by " <> t <> "."
    Gtk.mainQuit
    return ()

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

uiButtonPlayTwoPlayersClickHandler :: Gtk.Builder -> IO ()
uiButtonPlayTwoPlayersClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiTwoPlayersGame <- getBuilderObj builder "uiTwoPlayersGame" Gtk.Grid
    Gtk.stackSetVisibleChild uiStack uiTwoPlayersGame 
 
uiButtonSettingsClickHandler :: Gtk.Builder -> IO ()
uiButtonSettingsClickHandler builder = do
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiSettings <- getBuilderObj builder "uiSettings" Gtk.Box
    Gtk.stackSetVisibleChild uiStack uiSettings
    settingsObject <- LoadSettings.readSettingsFile
    
    -- previse imperativno, uradi na funkcionalni nacin
    Just uiEntry_igrac1_ime <- getBuilderObj builder "uiEntry_igrac1_ime" Gtk.Entry
    Just uiEntry_igrac1_boja <- getBuilderObj builder "uiEntry_igrac1_boja" Gtk.Entry
    Just uiEntry_igrac1_slika <- getBuilderObj builder "uiEntry_igrac1_slika" Gtk.Entry
    Just uiEntry_igrac2_ime <- getBuilderObj builder "uiEntry_igrac2_ime" Gtk.Entry
    Just uiEntry_igrac2_boja <- getBuilderObj builder "uiEntry_igrac2_boja" Gtk.Entry
    Just uiEntry_igrac2_slika <- getBuilderObj builder "uiEntry_igrac2_slika" Gtk.Entry
    Just uiEntry_duzina_igre_u_sekundama <- getBuilderObj builder "uiEntry_duzina_igre_u_sekundama" Gtk.Entry
    Just uiEntry_duzina_jednog_poteza_u_sekundama <- getBuilderObj builder "uiEntry_duzina_jednog_poteza_u_sekundama" Gtk.Entry
    Gtk.entrySetText uiEntry_igrac1_ime $ T.pack $ LoadSettings.getItem "igrac1_ime" settingsObject
    Gtk.entrySetText uiEntry_igrac1_boja $ T.pack $ LoadSettings.getItem "igrac1_boja" settingsObject
    Gtk.entrySetText uiEntry_igrac1_slika $ T.pack $ LoadSettings.getItem "igrac1_slika" settingsObject
    Gtk.entrySetText uiEntry_igrac2_ime $ T.pack $ LoadSettings.getItem "igrac2_ime" settingsObject
    Gtk.entrySetText uiEntry_igrac2_boja $ T.pack $ LoadSettings.getItem "igrac2_boja" settingsObject
    Gtk.entrySetText uiEntry_igrac2_slika $ T.pack $ LoadSettings.getItem "igrac2_slika" settingsObject
    Gtk.entrySetText uiEntry_duzina_igre_u_sekundama $ T.pack $ LoadSettings.getItem "duzina_igre_u_sekundama" settingsObject
    Gtk.entrySetText uiEntry_duzina_jednog_poteza_u_sekundama $ T.pack $ LoadSettings.getItem "duzina_jednog_poteza_u_sekundama" settingsObject

uiButtonBackFromSettingsClickHandler :: Gtk.Builder -> IO ()
uiButtonBackFromSettingsClickHandler builder = do
    
    Just uiEntry_igrac1_ime <- getBuilderObj builder "uiEntry_igrac1_ime" Gtk.Entry
    uiEntry_igrac1_ime_text <- Gtk.getEntryText uiEntry_igrac1_ime
    let uiEntry_igrac1_ime_str = T.unpack uiEntry_igrac1_ime_text
    
    Just uiEntry_igrac1_boja <- getBuilderObj builder "uiEntry_igrac1_boja" Gtk.Entry
    uiEntry_igrac1_boja_text <- Gtk.getEntryText uiEntry_igrac1_boja
    let uiEntry_igrac1_boja_str = T.unpack uiEntry_igrac1_boja_text
    
    Just uiEntry_igrac1_slika <- getBuilderObj builder "uiEntry_igrac1_slika" Gtk.Entry
    uiEntry_igrac1_slika_text <- Gtk.getEntryText uiEntry_igrac1_slika
    let uiEntry_igrac1_slika_str = T.unpack uiEntry_igrac1_slika_text
    
    Just uiEntry_igrac2_ime <- getBuilderObj builder "uiEntry_igrac2_ime" Gtk.Entry
    uiEntry_igrac2_ime_text <- Gtk.getEntryText uiEntry_igrac2_ime
    let uiEntry_igrac2_ime_str = T.unpack uiEntry_igrac2_ime_text
    
    Just uiEntry_igrac2_boja <- getBuilderObj builder "uiEntry_igrac2_boja" Gtk.Entry
    uiEntry_igrac2_boja_text <- Gtk.getEntryText uiEntry_igrac2_boja
    let uiEntry_igrac2_boja_str = T.unpack uiEntry_igrac2_boja_text
    
    Just uiEntry_igrac2_slika <- getBuilderObj builder "uiEntry_igrac2_slika" Gtk.Entry
    uiEntry_igrac2_slika_text <- Gtk.getEntryText uiEntry_igrac2_slika
    let uiEntry_igrac2_slika_str = T.unpack uiEntry_igrac2_slika_text
    
    Just uiEntry_duzina_igre_u_sekundama <- getBuilderObj builder "uiEntry_duzina_igre_u_sekundama" Gtk.Entry
    uiEntry_duzina_igre_u_sekundama_text <- Gtk.getEntryText uiEntry_duzina_igre_u_sekundama
    let uiEntry_duzina_igre_u_sekundama_str = T.unpack uiEntry_duzina_igre_u_sekundama_text
    
    Just uiEntry_duzina_jednog_poteza_u_sekundama <- getBuilderObj builder "uiEntry_duzina_jednog_poteza_u_sekundama" Gtk.Entry
    uiEntry_duzina_jednog_poteza_u_sekundama_text <- Gtk.getEntryText uiEntry_duzina_jednog_poteza_u_sekundama
    let uiEntry_duzina_jednog_poteza_u_sekundama_str = T.unpack uiEntry_duzina_jednog_poteza_u_sekundama_text
    
    
    let settingsObject = LoadSettings.Settings uiEntry_igrac1_ime_str uiEntry_igrac1_boja_str uiEntry_igrac1_slika_str uiEntry_igrac2_ime_str uiEntry_igrac2_boja_str uiEntry_igrac2_slika_str uiEntry_duzina_igre_u_sekundama_str uiEntry_duzina_jednog_poteza_u_sekundama_str
    LoadSettings.writeToSettingsFile settingsObject
    Just uiStack <- getBuilderObj builder "uiStack" Gtk.Stack
    Just uiMainMenu <- getBuilderObj builder "uiMainMenu" Gtk.Box
    Gtk.stackSetVisibleChild uiStack uiMainMenu
