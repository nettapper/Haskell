import Data.Map

input = "Every function in Haskell is a function in the mathematical sense ( i.e., pure ). Even side-effecting IO operations are but a description of what to do, produced by pure code. There are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers."

charCount = toList $ fromListWith (+) [(c,1) | c <- input]

wordCount = toList $ fromListWith (+) [(c,1) | c <- words input]
