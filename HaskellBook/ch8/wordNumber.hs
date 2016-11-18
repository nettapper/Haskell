module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
                     0 -> "Zero"
                     1 -> "One"
                     2 -> "Two"
                     3 -> "Three"
                     4 -> "Four"
                     5 -> "Five"
                     6 -> "Six"
                     7 -> "Seven"
                     8 -> "Eight"
                     9 -> "Nine"
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
