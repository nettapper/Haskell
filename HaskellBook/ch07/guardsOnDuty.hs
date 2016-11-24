avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | avg >= 0.9 = 'A'
  | avg >= 0.8 = 'B'
  | avg >= 0.7 = 'C'
  | avg >= 0.6 = 'D'
  | avg >= 0.5 = 'F'  -- On purpose wrong... Wall will complain about non-exhaustive patterns
  -- | avg >= 0.5 = 'F'  -- Wall will stil complain even though all patterns are covered
    where avg = x / 100

avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | otherwise  = 'Z'  -- we probs didn't want this here ;)
  | avg >= 0.9 = 'A'
  | avg >= 0.8 = 'B'
  | avg >= 0.7 = 'C'
  | avg >= 0.6 = 'D'
  | avg <  0.5 = 'F'
    where avg = x / 100

avgGrade'' :: (Fractional a, Ord a) => a -> Char
avgGrade'' x
  | avg >= 0.8 = 'B'   -- reordering doesn't produce errors but doesn't do what we would expect
  | avg >= 0.6 = 'D'
  | avg >= 0.7 = 'C'
  | avg <  0.5 = 'F'
  | avg >= 0.9 = 'A'
    where avg = x / 100

-- pal :: (Ord a, Foldable a) => a -> Bool
--  I don't need Ord, only equals...
--  I took a guess with Foldable but a list turned to be right due to reverse
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False


numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
