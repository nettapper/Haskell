module SemigroupExercises where

import Test.QuickCheck
import Data.Semigroup

semigroupAssc :: (Eq m, Semigroup m)
              => m -> m -> m -> Bool
semigroupAssc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Question 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssc = Trivial -> Trivial -> Trivial -> Bool
-- End Question 1
-- Question 2
newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
-- End Question 2

main :: IO ()
main = do
  quickCheck (semigroupAssc :: TrivialAssc)
  quickCheck (semigroupAssc :: IdentityAssc (Sum Int))
