data PlsFixMe a =
    FixMe
  | Pls a
  deriving (Show, Eq)

instance Functor PlsFixMe where
  fmap = error "Now it does complie"
