module Filtering where

-- 1)
multOfThree :: [Integer]
multOfThree = filter (\x -> mod x 3 == 0) [1..30]

-- 2)
howManyMultOfThree :: Int
howManyMultOfThree = length multOfThree

-- 3)
-- remove articles the/a/an
-- Prelude> myFilter "the brown dog was a goof"
--          ["brown","dog","was","goof"]

myFilter xs = filter (not . articles) $ words xs
  where articles word
          | word == "a" = True
          | word == "the" = True
          | word == "an" = True
          | otherwise = False
