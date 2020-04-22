module Types (
    Kolona (..), 
    Broj (..),
    Polje (..),
    PairWordIsOpened,
    Association (..),
    UI (..),
    saveUI,
    loadUI
) where


import System.IO 
import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import qualified GI.Gtk as Gtk
import Data.GI.Base

data Kolona = A | B | C | D  deriving (Eq, Show, Enum)

data Broj = F1 | F2 | F3 | F4 deriving (Eq, Show, Enum)

type Polje = (Kolona, Broj)

type PairWordIsOpened = (String, Bool)

data Association = Association {  a1_private :: PairWordIsOpened 
                                , a2_private :: PairWordIsOpened
                                , a3_private :: PairWordIsOpened
                                , a4_private :: PairWordIsOpened
                                , a_private  :: PairWordIsOpened
                                , b1_private :: PairWordIsOpened
                                , b2_private :: PairWordIsOpened
                                , b3_private :: PairWordIsOpened
                                , b4_private :: PairWordIsOpened
                                , b_private  :: PairWordIsOpened
                                , c1_private :: PairWordIsOpened
                                , c2_private :: PairWordIsOpened
                                , c3_private :: PairWordIsOpened
                                , c4_private :: PairWordIsOpened
                                , c_private  :: PairWordIsOpened
                                , d1_private :: PairWordIsOpened
                                , d2_private :: PairWordIsOpened
                                , d3_private :: PairWordIsOpened
                                , d4_private :: PairWordIsOpened
                                , d_private :: PairWordIsOpened
                                , final_private :: PairWordIsOpened } deriving (Show)


data UI = UI {    a1Button :: (Maybe Gtk.Button) 
                , a2Button :: (Maybe Gtk.Button)
                , a3Button :: (Maybe Gtk.Button)
                , a4Button :: (Maybe Gtk.Button)
                , aEntry   :: (Maybe Gtk.Entry)
                , b1Button :: (Maybe Gtk.Button)
                , b2Button :: (Maybe Gtk.Button)
                , b3Button :: (Maybe Gtk.Button)
                , b4Button :: (Maybe Gtk.Button)
                , bEntry   :: (Maybe Gtk.Entry)
                , c1Button :: (Maybe Gtk.Button)
                , c2Button :: (Maybe Gtk.Button)
                , c3Button :: (Maybe Gtk.Button)
                , c4Button :: (Maybe Gtk.Button)
                , cEntry   :: (Maybe Gtk.Entry)
                , d1Button :: (Maybe Gtk.Button)
                , d2Button :: (Maybe Gtk.Button)
                , d3Button :: (Maybe Gtk.Button)
                , d4Button :: (Maybe Gtk.Button)
                , dEntry   :: (Maybe Gtk.Entry)
                , finEntry :: (Maybe Gtk.Entry)
                , startGameButton :: (Maybe Gtk.Button)
                , settingsButton :: (Maybe Gtk.Button)
                , backButton :: (Maybe Gtk.Button)
                , nextButton :: (Maybe Gtk.Button)
                , quitButton :: (Maybe Gtk.Button)}


ui = UI {     a1Button = Nothing 
            , a2Button = Nothing
            , a3Button = Nothing
            , a4Button = Nothing
            , aEntry   = Nothing
            , b1Button = Nothing
            , b2Button = Nothing
            , b3Button = Nothing
            , b4Button = Nothing
            , bEntry   = Nothing
            , c1Button = Nothing
            , c2Button = Nothing
            , c3Button = Nothing
            , c4Button = Nothing
            , cEntry   = Nothing
            , d1Button = Nothing
            , d2Button = Nothing
            , d3Button = Nothing
            , d4Button = Nothing
            , dEntry   = Nothing
            , finEntry = Nothing
            , startGameButton = Nothing
            , settingsButton = Nothing
            , backButton = Nothing
            , nextButton = Nothing
            , quitButton = Nothing }


x = unsafePerformIO (newIORef ui)

loadUI :: UI
loadUI = unsafePerformIO $ readIORef x

saveUI :: UI -> IO ()
saveUI s = atomicModifyIORef x (\x -> (s, ()))