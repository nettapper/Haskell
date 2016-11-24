module MapFoldBool where

import Data.Bool

-- The bool func should do this
-- foldBool3 :: a -> a -> Bool -> a
-- foldBool3 x y True = x
-- foldBool3 x y False = y

-- write this with the bool func
target = map (\x -> if x == 3 then (-x) else (x)) [1..10]

solution = map (\x -> bool x (-x) (3 == x)) [1..10]
