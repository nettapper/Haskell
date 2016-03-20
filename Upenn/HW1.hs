import Data.Char

-- Validate Credit Cards

-- convert positive integers to list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = map toInteger $ map digitToInt (show n)

-- reverse toDigits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- double every number (left to right) starting w/ numb at position 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:ys) = x:(2*y):(doubleEveryOther ys)

-- calculate the sum of all numbers in list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

-- validate :: Integer -> Bool
