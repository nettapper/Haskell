module Anagram where

import Data.List (sort)

isAnagram :: String -> String -> Bool
isAnagram a b = (==) (sort a) (sort b)
