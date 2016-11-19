module MyHead where

myHead :: [a] -> a
myHead (x:_) = x

mySafeHead :: [a] -> Maybe a
mySafeHead []    = Nothing
mySafeHead (x:_) = Just x
