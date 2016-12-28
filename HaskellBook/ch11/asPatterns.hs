import Data.Char

isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _      = True
isSubsequence (x:xs) ys = elem x ys && isSubsequence xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords []       = []
capitalizeWords sentence = foldr capitalize [] $ words sentence
  where capitalize [] b       = b
        capitalize word@(a:as) b = (word, toUpper a : as) : b

