{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 22

newtype Pigs = Pigs Int deriving (Show, Eq)

instance TooMany Pigs where
  tooMany (Pigs n) = tooMany n

newtype Dogs = Dogs Int deriving (Show, Eq, TooMany)
