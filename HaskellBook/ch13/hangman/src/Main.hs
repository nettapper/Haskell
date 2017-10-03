module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type Wordlist = [String]

allWords :: IO Wordlist
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO Wordlist
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
    where gameLength w = let l = length (w :: String)
                          in l >= minWordLength && l < maxWordLength

randomWord :: Wordlist -> IO String
randomWord wl = do
  randomIndex <- randomRIO(0,length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String  -- word to guess
        [Maybe Char]  -- chars filled in thus far
        [Char]  -- letters guessed thus far

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
      ++ "Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (\x -> Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = c `elem` guesses

main :: IO ()
main = do
  putStrLn "hello world"
