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
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoAssc a b =
  (Two a b) -> (Two a b) -> (Two a b) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- End Question 3
-- Question 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj b <> BoolConj b' = BoolConj (b && b')

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssc = BoolConj -> BoolConj -> BoolConj -> Bool
-- End Question 4
-- Question 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj b <> BoolDisj b' = BoolDisj (b || b')

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
-- End Question 5
-- Question 6
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

data Hole = Hole

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- End Question 6
-- Question 7

-- End Question 7
-- Question 8

-- End Question 8
-- Question 9

-- End Question 9

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssc :: TrivialAssc)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "\n Identity"
  quickCheck (semigroupAssc :: IdentityAssc (Sum Int))
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  putStrLn "\n Two"
  quickCheck (semigroupAssc :: TwoAssc (Sum Int) Trivial)
  quickCheck (monoidRightIdentity :: Two (Sum Int) Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) Trivial -> Bool)
  putStrLn "\n BoolConj"
  quickCheck (semigroupAssc :: BoolConjAssc)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  putStrLn "\n BoolDisj"
  quickCheck (semigroupAssc :: BoolDisjAssc)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  putStrLn "\n Combine"
  let f = Combine $ \n -> Sum (n + 1)
  putStrLn $ show $ unCombine (mappend f mempty) $ 1

