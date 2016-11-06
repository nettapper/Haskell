module WorkingWithShow where

data Mood = Blah

instance Show Mood where
  show Blah = "Blah"
  -- show Blah = "Ke$ha - Blah Blah Blah"  -- I can also make it be rediculous things

data Mood' = Blah'
  deriving (Show)  -- Most of the time this will do what I want
