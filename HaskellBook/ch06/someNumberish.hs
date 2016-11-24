module SomeNumberish where

-- This is vacuous and silly. This is only to make a point. Please do not write typeclasses like this:
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

-- pretend newtype is data for now
newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

test = sumNumberish (Age 10) (Age 10)


-- What about if GHC cant infer the types?

-- This is even worse than the last one.
-- Don't use typeclasses to define default values.
class NumberishPrime a where
  fromNumber' :: Integer -> a
  toNumber' :: a -> Integer
  defaultNumber :: a

instance NumberishPrime Age where
  fromNumber' n = Age n
  toNumber' (Age n) = n
  defaultNumber = Age 65

instance NumberishPrime Year where
  fromNumber' n = Year n
  toNumber' (Year n) = n
  defaultNumber = Year 1988

sumNumberish' :: Numberish a => a -> a -> a
sumNumberish' a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

-- test' = sumNumberish' defaultNumber defaultNumber   -- this will error due to Ambiguous type
test'' = sumNumberish (defaultNumber :: Age) defaultNumber  -- this is fine
test''' = sumNumberish (defaultNumber :: Age) (defaultNumber :: Age)  -- and so is this
