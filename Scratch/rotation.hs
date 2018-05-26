-- built because Erica rotation riddles
-- can built a bin file w/ `stack ghc rotation.hs`
-- then run it w/ `./rotation`

numbers :: [Integer]
numbers = [100..999]

-- rotate x = Just $ rot x
-- could also define rotation like above...
-- where numbers that don't have a rotation stay the same
-- eg. 2 -> 2, while 6 -> 9
rotate :: Integer -> Maybe Integer
rotate x = if anyAreElem "23457" s
              then Nothing
              else Just $ rot x
  where s = show x
        rot = read . map replaceNum . reverse . show

-- True if any preds are in the string s
anyAreElem :: (Eq a) => [a] -> [a] -> Bool
anyAreElem [] _ = False
anyAreElem (p:ps) s = if p `elem` s
                         then True
                         else anyAreElem ps s

replaceNum :: Char -> Char
replaceNum c
  | c == '6' = '9'
  | c == '9' = '6'
  | otherwise = c

-- rotate(x) + 129 == x
rule2 :: Integer -> Bool
rule2 x = let r = rotate x
           in case r of
                   Nothing -> False
                   (Just n) -> n + 129 == x

-- rotate(x - 9) == x - 9
rule3 :: Integer -> Bool
rule3 x = let r = rotate (x - 9)
           in case r of
                   Nothing -> False
                   (Just n) -> n == x - 9

-- the solution must be a three digit number
-- and must satisfy rule2 and rule3
solution :: [Integer]
solution = filter (\x -> rule2 x && rule3 x) numbers

main :: IO ()
main = do
  putStrLn $ "Here lie the solution to your troubles:"
  putStrLn $ show $ solution
