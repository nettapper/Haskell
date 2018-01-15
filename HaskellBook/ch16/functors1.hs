data PlsFixMe =
    FixMe
  | Pls
  deriving (Show, Eq)

instance Functor PlsFixMe where
  fmap = error "Won't complie"

-- this won't complie but thats ok
-- checkout
-- :t fmap
-- :t (<$>)
-- :t ($)
