module WordNumber where

import Data.List (intersperse, intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
                     0 -> "zero"
                     1 -> "one"
                     2 -> "two"
                     3 -> "three"
                     4 -> "four"
                     5 -> "five"
                     6 -> "six"
                     7 -> "seven"
                     8 -> "eight"
                     9 -> "nine"
                     _ -> "INVALID INPUT"

digits :: Int -> [Int]
digits n = reverse (go n)
  where go x = if x > 0
                  then rem : go quo
                  else []
                    where (quo, rem) = divMod x 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits   -- a helpful suggestion from ghc-mod
-- wordNumber = concat . intersperse "-" . map digitToWord . digits

-- I'm really like pointfree, however I'm not yet writing it 'first' (I have to convert)
-- wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
