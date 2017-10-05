import Control.Monad (forever)
import System.Exit (exitSuccess, exitFailure)
import Data.Char (toLower, isAlpha)

-- you can use the following command to run from the cli w/o compilation!
-- stack runghc palidromeExitSuccess.hs

main :: IO ()
main = runPalindrome

runPalindrome :: IO ()
runPalindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
       True -> do
         putStrLn "It's a palindrome"
         exitSuccess
       False -> putStrLn "Nope!"

isPalindrome :: String -> Bool
isPalindrome p = (p' == reverse p')
  where p' = map toLower $ filter isAlpha p
