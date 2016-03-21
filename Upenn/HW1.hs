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



-- The Towers of Hanoi

-- To move n discs (stacked in increasing size) from peg a to peg b
-- using peg c as temporary storage,
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.

-- Example: hanoi 1 "a" "b" "c" == [("a","b")]
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- Example: hanoi 3 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b"), ("a","c"), ]

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numOfDisks start target temp
    | numOfDisks == 0 = []
    | numOfDisks == 1 = (start, target) : []
    | numOfDisks == 2 = (start, temp) : (start, target) : (temp, target) : []
    | otherwise = nextHanoi start temp target ++ [(start, target)] ++ nextHanoi temp target start
    where nextHanoi = hanoi (numOfDisks - 1)
