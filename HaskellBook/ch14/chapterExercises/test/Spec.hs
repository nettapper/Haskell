import TestWordNumber (run)
import Idempotence (run)
import Gen (run)
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Gen"
  Gen.run
  putStrLn "--------------"
  putStrLn "TestWordNumber"
  TestWordNumber.run
  putStrLn "--------------"
  putStrLn "Idempotence"
  Idempotence.run
  putStrLn "--------------"
  putStrLn "QuickCheck"
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
  quickCheck $ (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck $ (multCommutative :: Integer -> Integer -> Bool)
  quickCheck $ (quotLaw :: Integer -> Integer -> Bool)
  quickCheck $ (divLaw :: Integer -> Integer -> Bool)
  -- quickCheck $ (isCarrotAssc :: Integer -> Integer -> Integer -> Bool)  -- no, uncomment to see yourself
  -- quickCheck $ (isCarrotComm :: Integer -> Integer -> Bool)  -- and no, uncomment to see yourself
  quickCheck $ (rList :: [Int] -> Bool)
  quickCheck $ (fDollar id :: Int -> Bool)
  quickCheck $ (fDollar (+ 10) :: Int -> Bool)
  quickCheck $ (fDollar (* 2323) :: Integer -> Bool)
  quickCheck $ (fDot id id :: Integer -> Bool)
  quickCheck $ (fDot (* 2) (^ 3) :: Integer -> Bool)
  -- quickCheck $ (areFoldsEqual :: [Integer] -> [Integer] -> Bool)  -- answer, no
  quickCheck $ (areFoldsEqual' :: [[Integer]] -> Bool)
  quickCheck $ (areFoldsEqual' :: [[String]] -> Bool)
  -- quickCheck $ (f :: Int -> [Integer] -> Bool) -- this one took a while but I guess not
  quickCheck $ (read'n'show :: Int -> Bool)
  quickCheck $ ((\x -> squareIdentity x == square x) :: Double -> Bool)  -- I'll leave the last one failing!

square x = x * x
squareIdentity = square . sqrt

read'n'show x = (read (show x)) == x

f n xs = length (take n xs) == n

areFoldsEqual' as = foldr (++) [] as == concat as

areFoldsEqual as bs = (++) as bs == foldr (:) as bs

fDot f g x = (f . g) x == f (g x)

fDollar :: (Eq b) => (a -> b) -> a -> Bool
fDollar f a = (f a) == (f $ a)

rList xs = reverse (reverse xs) == id xs

isCarrotAssc x y z = (x ^ y) ^ z == x ^ (y ^ z)
isCarrotComm x y = x ^ y == y ^ x

quotLaw _ 0 = True  -- quot by 0
quotLaw x y = (quot x y) * y + (rem x y) == x
divLaw _ 0 = True  -- div by 0
divLaw x y = (div x y) * y + (mod x y) == x

multAssociative x y z = (x * y) * z == x * (y * z)
multCommutative x y = x * y == y * x

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
