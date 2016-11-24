myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr $ map f xs

myElm :: Eq a => a -> [a] -> Bool
myElm _ [] = False
myElm x (y:ys)
  | x == y = True
  | otherwise = myElm x ys

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = myMaxByHelper f x xs
  where myMaxByHelper _ current [] = current
        myMaxByHelper g current (y:ys) = case (g current y) of
                                              GT -> myMaxByHelper g current ys
                                              EQ -> myMaxByHelper g current ys
                                              LT -> myMaxByHelper g y ys  -- new highest
