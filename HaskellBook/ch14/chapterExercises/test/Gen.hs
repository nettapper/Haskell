module Gen where

import Test.QuickCheck

run :: IO ()
run = do
  sample genFool
  putStrLn "~~~~"
  sample genFulse

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFulse :: Gen Fool
genFulse = frequency [ (2, return Fulse)
                     , (1, return Frue)]
