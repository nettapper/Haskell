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

-- doubleEveryOther :: [Integer] -> [Integer]
-- sumDigits :: [Integer] -> Integer
-- validate :: Integer -> Bool
