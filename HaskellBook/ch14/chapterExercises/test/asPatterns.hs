module AsPatterns where

import Data.Char (toUpper)

isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _      = True
isSubsequence (x:xs) ys = elem x ys && isSubsequence xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords []       = []
capitalizeWords sentence = foldr capitalize [] $ words sentence
  where capitalize [] b       = b
        capitalize word@(a:as) b = (word, toUpper a : as) : b

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs = unwords $ capNextWord $ words xs

capNextWord []       = []
capNextWord [x]      = [x]
capNextWord (x:y:xs) = if null x
                          then x : capNextWord (y:xs)
                          else if last x == '.'
                            then x : capitalizeWord y : capNextWord xs
                            else x : capNextWord (y:xs)

capitalizeParagraph' :: String -> String
capitalizeParagraph' []     = []
capitalizeParagraph' (x:xs) = if x == '.'
                                 then x : capNextLet xs
                                 else x : capitalizeParagraph xs
                                   where capNextLet []       = []
                                         capNextLet (' ':xs) = ' ' : capNextLet xs
                                         capNextLet (x:xs)   = toUpper x : capitalizeParagraph xs
