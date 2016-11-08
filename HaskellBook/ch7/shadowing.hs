module Shadowing where

-- Note: x will only be bound to 10 due to shadowing
bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
                "the integer was: " ++ show x
                ++ " and y was: " ++ show y

-- Haskell is lexically scoped

first = bindExp 10002

second = bindExp 3

third = first == second  -- True
