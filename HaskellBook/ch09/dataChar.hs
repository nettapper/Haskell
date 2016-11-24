module DataChar where

import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

holler :: String -> String
holler [] = []
holler (x:xs) = toUpper x : holler xs

holler' = map toUpper

capitalizeHead :: String -> Char
capitalizeHead xs = toUpper $ head xs

capitalizeHeadComposed :: String -> Char
capitalizeHeadComposed xs = toUpper . head $ xs

capitalizeHeadPF :: String -> Char
capitalizeHeadPF = toUpper . head
