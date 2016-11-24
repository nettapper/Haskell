module SquareCube where

mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuples = [(x,y) | x <- mySqr, y <- myCube]

myTuplesLessThanFifty = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- [ Output | Input ]
-- [ x | x <- xs ] means x is drawn from xs
-- [ x | x <- xs, True / False ] predicates can be added an must return a Bool
-- [ (x,y) | x <- xs, y <- ys ] we will exhaust the right most first

howManyTuples = length myTuplesLessThanFifty
