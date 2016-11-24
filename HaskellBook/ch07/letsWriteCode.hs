module LetsWriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = divMod x 10
        (_, d)     = divMod xLast 10

hunsD x = d2
  where (xLast, _) = divMod x 10
        (xLast', _) = divMod xLast 10
        (_, d2) = divMod xLast' 10


foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of  -- syntastic just complaning, why not if-then-else
                      True -> x
                      False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  -- | b == True = x  -- same as the one above
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g fab (a, c) = (fab a, c)
