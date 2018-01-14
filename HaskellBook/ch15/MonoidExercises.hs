module MonoidExercises where

import Test.QuickCheck
import Data.Semigroup (Semigroup, (<>), Sum(..))
import Data.Monoid (Monoid)

semigroupAssc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mappend mempty a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mappend a mempty) == a

-- Question 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssc =
  Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial
-- End Question 1
-- Question 2
newtype Identity a =
  Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

data Hole = Hole

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityAssc a =
  (Identity a) -> (Identity a) -> (Identity a) -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
-- End Question 2
-- Question 3

-- End Question 3
-- Question 4

-- End Question 4
-- Question 5

-- End Question 5
-- Question 6

-- End Question 6
-- Question 7

-- End Question 7
-- Question 8

-- End Question 8
-- Question 9

-- End Question 9

main :: IO ()
main = do
  quickCheck (semigroupAssc :: TrivialAssc)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssc :: IdentityAssc (Sum Int))
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)

