{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (i, s) = i > 42

instance TooMany (Int, Int) where
  tooMany (i, j) = (i + j) > 42

-- So I didn't quite get this, but nice to know that the
-- answers are avalable on githubs
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, t) = tooMany (negate n) && tooMany t
