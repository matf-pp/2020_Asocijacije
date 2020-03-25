module Strings where

import Data.Char

equalStrings :: [Char] -> [Char] -> Bool
equalStrings a b = processString a == processString b

processString :: [Char] -> [Char]
processString x = concat $ fmap (toLat . toLower) x

toLat :: Char -> [Char]
toLat 'а' = "a"
toLat 'б' = "b"
toLat 'в' = "v"
toLat 'г' = "g"
toLat 'д' = "d" 
toLat 'е' = "e"
toLat 'ђ' = "dj"
toLat 'ж' = "z"
toLat 'з' = "z"
toLat 'и' = "i"
toLat 'ј' = "j"
toLat 'к' = "k"
toLat 'л' = "l"
toLat 'м' = "m"
toLat 'н' = "n"
toLat 'њ' = "nj"
toLat 'о' = "o"
toLat 'п' = "p"
toLat 'р' = "r"
toLat 'с' = "s"
toLat 'т' = "t"
toLat 'у' = "u"
toLat 'ф' = "f"
toLat 'х' = "h"
toLat 'ц' = "c"
toLat 'ч' = "c"
toLat 'џ' = "dz"
toLat 'ш' = "s"
toLat 'č' = "c"
toLat 'ć' = "c" 
toLat 'š' = "s"
toLat 'đ' = "dj"
toLat 'ž' = "z"
toLat  x  = [x]
