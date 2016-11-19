module MyEnumFromTo where

eftBool :: Bool -> Bool -> [Bool]  -- kinda cheating I know
eftBool True True   = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = go a b []

eftInt :: Int -> Int -> [Int]
eftInt a b = go a b []

eftChar :: Char -> Char -> String
eftChar a b = go a b []

-- my helper func that I noticed I was rewriting...
go :: (Enum a, Ord a) => a -> a -> [a] -> [a]
go x y l
  | x > y = l
  | x == y = l ++ [x]
  | otherwise = go (succ x) y (l ++ [x])
