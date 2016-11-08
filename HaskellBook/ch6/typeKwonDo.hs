chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (==) b (f a)

-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = (+) (f a) (fromInteger i)
