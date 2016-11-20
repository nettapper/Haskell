module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
myLines x = myLinesHelper x []

myLinesHelper :: String -> [String] -> [String]
myLinesHelper [] xs = xs
myLinesHelper x xs = myLinesHelper (dropLine x) (xs ++ [takeLine x])
  where dropLine = dropWhile (== '\n') . dropWhile (/= '\n')
        takeLine = takeWhile (/= '\n')

-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $ "Are they equal? "
        ++ show (myLines sentences == shouldEqual)

