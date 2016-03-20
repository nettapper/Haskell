import Data.Char

-- Validate Credit Cards

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = map toInteger $ map digitToInt (show n)

-- toDigitsRev :: Integer -> [Integer]
-- doubleEveryOther :: [Integer] -> [Integer]
-- sumDigits :: [Integer] -> Integer
-- validate :: Integer -> Bool
