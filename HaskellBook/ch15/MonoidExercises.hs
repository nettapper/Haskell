module MonoidExercises where

import Test.QuickCheck
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)

semigroupAssc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdenity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdenity a = (mappend mempty a) == a

monoidRightIdenity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdenity a = (mappend a mempty) == a

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

main :: IO ()
main = do
  let sa = semigroupAssc
      mli = monoidLeftIdenity
      mri = monoidRightIdenity
  quickCheck (sa :: TrivialAssc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)

