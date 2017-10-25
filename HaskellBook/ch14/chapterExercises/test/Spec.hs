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
  quickCheck $ (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck $ (multCommutative :: Integer -> Integer -> Bool)
  quickCheck $ (quotLaw :: Integer -> Integer -> Bool)
  quickCheck $ (divLaw :: Integer -> Integer -> Bool)
  -- quickCheck $ (isCarrotAssc :: Integer -> Integer -> Integer -> Bool)  -- no, uncomment to see yourself
  -- quickCheck $ (isCarrotComm :: Integer -> Integer -> Bool)  -- and no, uncomment to see yourself

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