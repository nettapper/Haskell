module OptionalMonoid where

import Test.QuickCheck
import Data.Monoid ((<>), Sum(..), Product(..))

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = mempty
  mappend Nada (Only a) = Only (a <> mempty)
  mappend (Only a) Nada = Only (a <> mempty)
  mappend (Only a) (Only b) = Only (a <> b)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = genOptional

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional = do
    a <- arbitrary
    frequency [(10, return $ Only a)
              ,(1, return $ Nada)]

main' :: IO ()
main' = do
  putStrLn $ show $ Only (Sum 1) `mappend` Only (Sum 1)
  putStrLn $ show $ Only (Product 4) `mappend` Only (Product 2)
  putStrLn $ show $ Only (Sum 1) `mappend` Nada
  putStrLn $ show $ Only [1] `mappend` Nada
  putStrLn $ show $ Nada `mappend` Only (Sum 1)
