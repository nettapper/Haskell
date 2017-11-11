module SemigroupExercises where

import Test.QuickCheck
import Data.Semigroup

-- Question 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssc :: (Eq m, Semigroup m)
              => m -> m -> m -> Bool
semigroupAssc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssc = Trivial -> Trivial -> Trivial -> Bool
-- End Question 1

main :: IO ()
main = quickCheck (semigroupAssc :: TrivialAssc)
