data PlsFixMe a =
    FixMe
  | Pls a
  deriving (Show, Eq)

instance Functor PlsFixMe where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls $ f a

-- λ> fmap (+1) (Pls 1)
-- Pls 2
