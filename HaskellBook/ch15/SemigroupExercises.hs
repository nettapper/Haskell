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
-- Question 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool
-- End Question 5
-- Question 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

instance Semigroup BoolConj where
  (BoolConj b) <> (BoolConj b') = BoolConj (b && b')

type BoolConjAssc = BoolConj -> BoolConj -> BoolConj -> Bool
-- End Question 6
-- Question 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
type BoolDisjAssc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

instance Semigroup BoolDisj where
  (BoolDisj b) <> (BoolDisj b') = BoolDisj $ b || b'
-- End Question 7
-- Question 8
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)
type OrAssc a b = Or a b -> Or a b -> Or a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a)
              ,(1, return $ Snd b)]

instance Semigroup (Or a b) where
  (Fst a) <> (Fst a') = Fst a
  (Snd b) <> (Fst a) = Snd b
  (Fst a) <> (Snd b) = Snd b
  (Snd b) <> (Snd b') = Snd b
-- End Question 8
-- Question 9
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine b) <> (Combine b') = Combine (b <> b')

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    b <- arbitrary
    return $ Combine b
-- End Question 9
-- Question 10
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp $ a . a'

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a
-- End Question 10
-- Question 11
data Validation a b =
    Fail a
  | Succ b
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (Fail b) <> (Fail b') = Fail $ b <> b'
  (Succ a) <> _ = Succ a
  _ <> (Succ a) = Succ a
-- End Question 11

main :: IO ()
main = do
  quickCheck (semigroupAssc :: TrivialAssc)
  quickCheck (semigroupAssc :: IdentityAssc (Sum Int))
  quickCheck (semigroupAssc :: TwoAssc (Sum Int) (Product Int))
  quickCheck (semigroupAssc :: ThreeAssc (Sum Int) (Product Int) [Int])
  quickCheck (semigroupAssc :: FourAssc [Float] (Sum Int) (Product Int) [Int])
  quickCheck (semigroupAssc :: BoolConjAssc)
  quickCheck (semigroupAssc :: BoolDisjAssc)
  quickCheck (semigroupAssc :: OrAssc (Sum Integer) (Product Double))
  let f = Combine $ (\n -> Sum (n + 1))
  let g = Combine $ (\n -> Sum (n - 1))
  putStrLn $ show $ unCombine (f <> g) 0 == 0
  putStrLn $ show $ unCombine (f <> g) 1 == 2
  putStrLn $ show $ unCombine (f <> f) 1 == 4
  putStrLn $ show $ unCombine (g <> f) 1 == 2
  let h = Comp (\a -> a + (1 :: Int))
  putStrLn $ show $ unComp (h <> h) 1 == 3
  let i = Comp (\a -> a + (10 :: Int))
  putStrLn $ show $ unComp (h <> i) 1 == 12
  let failure :: String -> Validation String Int
      failure = Fail
      success :: Int -> Validation String Int
      success = Succ
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

