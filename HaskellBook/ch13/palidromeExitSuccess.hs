import Control.Monad (forever)
import System.Exit (exitSuccess)

-- you can use the following command to run from the cli w/o compilation!
-- stack runghc palidromeExitSuccess.hs

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
       True -> do
         putStrLn "It's a palindrome"
         exitSuccess
       False -> putStrLn "Nope!"

main :: IO ()
main = palindrome

