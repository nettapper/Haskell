-- typeInference1.hs
module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

f' x y = x + y

f'' :: Int -> Int -> Int
f'' x y = x + y

-- look at the types of f & f' & f''
-- ... no change
