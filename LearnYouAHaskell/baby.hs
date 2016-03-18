
alwaysTrue = True /= False

double x = x*2
-- doubleUs x y = x*2 + y*2
doubleUs x y = double x + double y
doubleSmallNumber x = if x < 100
                      then double x
                      else x
love = "I love haskell"
putTogether = [1,2,3,4] ++ [5,6,7,8]
putTogether' = 1 : [2,3,4,5,6,7,8]
getPutTogether x = putTogether !! x
putMyHead = print $ head putTogether  -- only the first
putMyTail = print (tail putTogether)  -- the rest (not the first)
putMyLast = print $ last putTogether  -- only the last
putMyInit = print (init putTogether)  -- the others (not the last)

alpha = ['a' .. 'z']  ++ ['A' .. 'Z']

listComp = [x*2 | x <- [1..100], x*2 >= 10, x*2 < 100]
listComp' = [x*y | x <- [1..5], y <- [10..15],
                        x > 2]  -- testing list comp on multilple lines (it works!)

length' :: Num a => [t] -> a
length'  xs = sum [1 | _ <- xs]  -- lenght with list comp

removeAlpha xs = [x | x <- xs, not (elem x alpha)]  -- alpha defined above as upper & lower case letters
