-- recursive sum
mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = mySum (n - 1) + n

-- multiplication by sum
myMult :: (Integral a) => a -> a -> a
myMult 1 y = y
myMult x 1 = x
myMult x y = myMult x (y - 1) + x

data DividedResult =
    Result Integer
  | DividedByZero
  deriving (Show)

-- fixing partial func
dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0     = DividedByZero
dividedBy num dem = go num dem 0
  where go n d count
          | n < d     = Result count
          | otherwise = go (n - d) d (count + 1)

-- The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise.
-- The function is recursive.

mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ n + 11

test_mc91 = map mc91 [95..110] == [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
