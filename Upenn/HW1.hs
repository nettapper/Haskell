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
--  [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x >= 10 = sumDigits (toDigits x) + sumDigits xs
    | otherwise = x + sumDigits xs

-- double every second digit (right to left), sum the digits,
-- calculate the remainder and return true if its 0
validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigitsRev n) 10  == 0
