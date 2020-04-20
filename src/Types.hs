module Types (
    Kolona (..), 
    Broj (..),
    KolonaBroj,
    Polje (..),
    PairWordIsOpened,
    Association (..)
) where

data Kolona = A | B | C | D  deriving (Eq, Show, Enum)

data Broj = F1 | F2 | F3 | F4 deriving (Eq, Show, Enum)

type KolonaBroj = (Kolona, Broj)

data Polje = Konacno (Maybe Kolona) | NijeKonacno KolonaBroj 

type PairWordIsOpened = (String, Bool)

data Association = Association   { a1_private :: PairWordIsOpened 
                                 , a2_private :: PairWordIsOpened
                                 , a3_private :: PairWordIsOpened
                                 , a4_private :: PairWordIsOpened
                                 , a_private :: PairWordIsOpened
                                 , b1_private :: PairWordIsOpened
                                 , b2_private :: PairWordIsOpened
                                 , b3_private :: PairWordIsOpened
                                 , b4_private :: PairWordIsOpened
                                 , b_private :: PairWordIsOpened
                                 , c1_private :: PairWordIsOpened
                                 , c2_private :: PairWordIsOpened
                                 , c3_private :: PairWordIsOpened
                                 , c4_private :: PairWordIsOpened
                                 , c_private :: PairWordIsOpened
                                 , d1_private :: PairWordIsOpened
                                 , d2_private :: PairWordIsOpened
                                 , d3_private :: PairWordIsOpened
                                 , d4_private :: PairWordIsOpened
                                 , d_private :: PairWordIsOpened
                                 , final_private :: PairWordIsOpened
                                 } deriving (Show)
