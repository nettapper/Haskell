data Mood = Blah
          | Woot
          | Clar
          | Mahhh
          deriving (Show, Enum)  -- by adding Enum I can use [ Blah .. Mahhh ], wow!
          -- deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0
             then negate x
             else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a,b) (c,d) = ((b,d), (a,c))


-- Fix the errors in the code below
--

-- Should add one to the length of a string
x = (+)
ff xs = w `x` 1
  where w = length xs

-- Should be the identity function
identity x = x
-- or I can use as a lambda
-- (\ x -> x)

-- Given [1,2,3] this should return 1
start = (\ (x:xs) -> x)

-- Should return 1 from (1,2)
fff (a,b) = a

