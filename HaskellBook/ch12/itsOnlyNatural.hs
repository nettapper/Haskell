-- As natural as any competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero     = (0 :: Integer)
natToInteger (Succ n) = 1 + natToInteger n

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i = if i < 0
                    then Nothing
                    else Just $ go i
                      where go :: Integer -> Nat
                            go 0 = Zero
                            go i = Succ $ go $ i - 1

-- Needed to make GhcMod work!
main :: IO()
main = return ()
