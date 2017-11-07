-- Can compile with "stack ghc MaybeAnotherMonoid.hs"
module Main where

import OptionalMonoid

import Data.Monoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = undefined
  mappend _ _ = undefined

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = genFirst'

genFirst' :: (Arbitrary a) => Gen (First' a)
genFirst' = do
    a <- arbitrary
    return $ First' a

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidAssc :: (Eq m, Monoid m)
           => m -> m -> m -> Bool
monoidAssc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a =
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
