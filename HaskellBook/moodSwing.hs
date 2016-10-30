data Mood = Blah
          | Woot
          | Clar
          | Mahhh
          deriving (Show, Enum)  -- by adding Enum I can use [ Blah .. Mahhh ], wow!
          -- deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
