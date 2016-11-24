module Ciphers where

import Data.Char

caesar :: String -> String
caesar = map $ cipher 3

cipher :: Int -> Char -> Char
cipher n = chr . denormilize . keepInRange . shift n . normilize . ord
  where shift = (+)
        keepInRange c = mod c (ord 'z' - ord 'a' + 1)  -- 26 for the 26 letters
        normilize c = c - ord 'a'
        denormilize c = c + ord 'a'

unCaesar :: String -> String
unCaesar = map $ cipher (-3)
