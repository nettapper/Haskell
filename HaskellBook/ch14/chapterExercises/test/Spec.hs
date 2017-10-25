import TestWordNumber (run)
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "TestWordNumber"
  run
  putStrLn "--------------"
  usingQC

usingQC :: IO ()
usingQC = do
  quickCheck $ \x -> x == halfIdentity (x :: Double)

half x = x / 2

halfIdentity = (*2) . half
