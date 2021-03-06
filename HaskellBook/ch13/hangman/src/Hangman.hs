module Hangman where

import Data.List (intersperse)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 7

data Puzzle =
  Puzzle String  -- word to guess
        [Maybe Char]  -- chars filled in thus far
        [Char]  -- letters guessed thus far
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed ++ "."

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (\_ -> Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = c `elem` guesses

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
               then Just wordChar
               else guessChar
          newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ guess:[]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You alread guessed that\
                  \ character, pick\
                  \ something else!"
        return puzzle
      (True, _) -> do
        putStrLn "The character was in the\
                  \ word, filling in the\
                  \ puzzle accordly."
        return (fillInCharacter puzzle guess)
      (False, _) -> do
        putStrLn "The character wasn't in the\
                  \ word, please try again."
        return (fillInCharacter puzzle guess)
