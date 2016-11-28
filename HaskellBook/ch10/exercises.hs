stops  = "pbtdkg"
vowels = "aeiou"

threeTupleMaker :: String -> String -> [(Char, Char, Char)]
threeTupleMaker stops vowels = permThree stops vowels stops
  where permThree :: String -> String -> String -> [(Char, Char, Char)]
        permThree xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

threeTupleMaker' :: [(Char, Char, Char)]
threeTupleMaker' = filter firstMustHaveP $ threeTupleMaker stops vowels
  where firstMustHaveP :: (Char, Char, Char) -> Bool
        firstMustHaveP (c, _, _) = c == 'p'

nouns = ["dog", "cat"]
verbs = ["run", "eat", "fly"]

threeTupleMaker'' :: [a] -> [a] -> [(a, a, a)]
threeTupleMaker'' stops vowels = permThree stops vowels stops
  where permThree :: [a] -> [a] -> [a] -> [(a, a, a)]
        permThree xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

-- this will find the avg word length
-- seekritFunc $ unwords verbs
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' :: String -> Double
seekritFunc' x =
  (/) (fromInteger $ toInteger (sum (map length (words x))))
      (fromInteger $ toInteger (length (words x)))

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' xs = foldr (||) False xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x
                    then True
                    else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs) = f x || myAny f xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f xs = foldr (cond f) False xs
  where cond :: (a -> Bool) -> a -> Bool -> Bool
        cond f a b = f a || b
