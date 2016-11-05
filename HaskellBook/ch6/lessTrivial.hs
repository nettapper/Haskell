-- you can set -Wall in ghci with
-- :set -Wall

module LessTrivial where

data DayOfTheWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving Show

data Date =
  Date DayOfTheWeek Int
  deriving Show

instance Eq DayOfTheWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False  -- Wall will complain if I leave this out

iHateMondays = Mon == Mon

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

spooky = Date Fri 13

isItSpooky = spooky == spooky
