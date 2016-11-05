module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
    where woot :: Integer
          woot = 10  -- woot isn't a top level definition

topLevelValue :: Integer
topLevelValue = 5
