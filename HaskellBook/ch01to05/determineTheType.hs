{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType

where -- simple example

example = 1

-- run the following in ghci to check that I got the types right

a = (* 9) 6
b = head [(0,"doge"),(1,"kitteh")]
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

x' = 5
y' = x' + 5
w = y' * 10
-- What is the type of w?

x = 5
y = x + 5
-- y = x + 5 :: Int  -- just testing, as I thought, this y isn't used in the func z
z y = y * 10
-- What is the type of z?

x'' = 5
y'' = x'' + 5
f'' = 4 / y''
-- What is the type of f''?

x''' = "Julie"
y''' = " <3 "
z''' = "Haskell"
f''' = x''' ++ y''' ++ z'''
-- What is the type of f?
