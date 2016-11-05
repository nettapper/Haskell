module LessTrivial where

data DayOfTheWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfTheWeek Int

instance Eq DayOfTheWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

iHateMondays = Mon == Mon

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

spooky = Date Fri 13

isItSpooky = spooky == spooky
