module Lambdas where

-- 1. Which (two or more) of the following are equivalent?
mTha x y z = x * y * z
mThb x y = \z -> x * y * z
mThc x = \y -> \z -> x * y * z
mThd = \x -> \y -> \z -> x * y * z
-- A: Trick question, they're all equal

-- 2. The type of mTh (above) is Num a => a -> a -> a -> a. Which is the type of mTh 3?
-- a) Integer -> Integer -> Integer
-- b) Num a => a -> a -> a -> a
-- c) Num a => a -> a
-- d) Num a => a -> a -> a  -- dis one

-- Rewrite in lambda syntax

-- addOneIfOdd n = case odd n of
--                      True -> f n
--                      False -> n
--                        where f n = n + 1
addOneIfOdd' n = case odd n of
                     True -> (\n -> n + 1) n
                     False -> n

-- addFive x y = (if x > y then y else x) + 5
addFive = (\x -> (\y -> if x > y then y + 5 else x + 5))


mflip f = \x -> \y -> f y x
mflip' f x y  = f y x  -- Be careful of the order of things
