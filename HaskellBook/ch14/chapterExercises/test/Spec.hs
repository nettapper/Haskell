import TestWordNumber (run)
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = do
  putStrLn "TestWordNumber"
  run
  putStrLn "--------------"
  usingQC

usingQC :: IO ()
usingQC = do
  quickCheck $ \x -> x == halfIdentity (x :: Double)
  quickCheck $ ((listOrdered . sort) :: [Integer] -> Bool)
  quickCheck $ ((listOrdered . sort) :: [String] -> Bool)
  quickCheck $ ((listOrdered . sort) :: [Int] -> Bool)
  quickCheck $ (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck $ (plusCommutative :: Int -> Int -> Bool)
  -- quickCheck $ (plusAssociative :: Float -> Float -> Float -> Bool)  -- Wont work


plusAssociative x y z = (x + y) + z == x + (y + z)
plusCommutative x y = x + y == y + x

-- for any sorted list this should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where go _ status@(_, False) = status
       go y (Nothing, t) = (Just y, t)
       go y (Just x, t) = (Just y, x >= y)

half x = x / 2
halfIdentity = (*2) . half
