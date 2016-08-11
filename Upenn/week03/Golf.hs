-- CIS 194 Homework 2

module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips l  = skips' l (length l -1)

skips' :: [a] -> Int -> [[a]]
skips' l n
  | n <= 0 = [l]
  | otherwise = skips' l (n-1) ++ [dropper l n]

dropper :: [a] -> Int -> [a]
dropper [] _ = []
dropper l  n = take 1 (drop n l) ++ dropper (drop (n+1) l) n
