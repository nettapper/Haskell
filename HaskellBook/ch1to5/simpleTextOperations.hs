module SimpleTextOperations where

a :: String -> String
a s = s ++ "!"

b :: String -> String
b _ = "y"

c :: String -> String
c s = words s !! 2
-- c s = head $ drop 2 $ words s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i
