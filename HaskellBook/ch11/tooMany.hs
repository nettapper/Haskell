class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int
  deriving (Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 1

myGoats = tooMany (Goats 10)

type Cows = Int

-- instance TooMany Cows where  -- Can't do this
--   tooMany n = n > 100
