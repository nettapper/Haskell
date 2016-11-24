-- you can set -Wall in ghci with
-- :set -Wall

module LessTrivial where

data DayOfTheWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Show)
  -- deriving (Eq)  -- I could also now use Eq rather than my own implementation
  -- deriving (Ord)  -- when using ord Mon is less than Tue because Mon is on the left

data Date =
  Date DayOfTheWeek Int
  deriving Show

-- As a general note, I need to ensure that Eq and Ord make sense w/ respect to eachother!
instance Eq DayOfTheWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False  -- Wall will complain if I leave this out

instance Ord DayOfTheWeek where  -- I like Fridays
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

iHateMondays = Mon == Mon

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

spooky = Date Fri 13

isItSpooky = spooky == spooky

-- Will they work??

v1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
v2 = compare (3 * 4) (3 * 5)
-- v3 = compare "Julie" True  -- this one doesn't
v4 = (5 + 3) > (3 + 6)
