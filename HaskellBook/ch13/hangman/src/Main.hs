module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w = let l = length (w :: String)
                          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
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
      ++ " Guessed so far: " ++ guessed ++ "."

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (\x -> Nothing) word) []

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

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = do
  if (length guessed) >= maxWordLength
     then do putStrLn "You lose!"
             putStrLn $ "The word was: " ++ wordToGuess
             exitSuccess
     else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
     then do putStrLn "You win!!"
             exitSuccess
     else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
       [c] -> handleGuess puzzle c >>= runGame
       _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering  -- need to make the prompt show up initially
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
