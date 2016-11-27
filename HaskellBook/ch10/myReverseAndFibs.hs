-- just def'ning my own rev funcs

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
revl :: Foldable t => t a -> [a]
-- revl = foldl (\b a -> [a] ++ b) []
revl = foldl (flip (:)) []

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
revr :: Foldable t => t a -> [a]
revr = foldr (\a b -> b ++ [a]) []

-- the difference between foldl and foldr is apparent


myFibs :: [Integer]
myFibs = 1 : scanl (+) 1 myFibs

myFibsN :: Int -> Integer
myFibsN index = myFibs !! index

myFibs20 :: [Integer]
myFibs20 = take 20 $ 1 : scanl (+) 1 myFibs

myFibsU100 :: [Integer]
myFibsU100 = takeWhile (< 100) myFibs

myFact :: [Integer]
myFact = scanl (*) 1 [2..]
