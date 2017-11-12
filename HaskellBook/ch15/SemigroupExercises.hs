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
--
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
-- Question 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
-- End Question 3
-- Question 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
-- End Question 4

main :: IO ()
main = do
  quickCheck (semigroupAssc :: TrivialAssc)
  quickCheck (semigroupAssc :: IdentityAssc (Sum Int))
  quickCheck (semigroupAssc :: TwoAssc (Sum Int) (Product Int))
  quickCheck (semigroupAssc :: ThreeAssc (Sum Int) (Product Int) [Int])
