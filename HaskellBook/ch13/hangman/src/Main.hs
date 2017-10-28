module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Hangman

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

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
gameOver (Puzzle wordToGuess filledInSoFar guessed) = do
  let numberOfGuesses filledInSoFar guessed = length guessed - correctGuesses filledInSoFar
        where correctGuesses ms = length $ filter isJust ms
   in if (numberOfGuesses filledInSoFar guessed) >= maxWordLength
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
