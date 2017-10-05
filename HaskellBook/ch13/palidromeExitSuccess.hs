import Control.Monad (forever)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
       True -> do
         putStrLn "It's a palindrome"
         exitSuccess
       False -> putStrLn "Nope!"

