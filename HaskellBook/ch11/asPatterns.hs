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
capitalizeParagraph []     = []
capitalizeParagraph (x:xs) = if x == '.'
                             then x : capNextLet xs
                             else x : capitalizeParagraph xs
                               where capNextLet []       = []
                                     capNextLet (' ':xs) = ' ' : capNextLet xs
                                     capNextLet (x:xs)   = toUpper x : capitalizeParagraph xs
