module DoesItTypeCheck where

-- 1
data Person = Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- 2
data Mood = Blah
          | Woot
          deriving (Show, Eq)

settleDown :: Mood -> Mood  -- 3
settleDown x = if x == Woot
                  then Blah
                  else x

-- Blah > Woot will require derving / creating an instance for Ord


-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"  -- this does type check .... partial data type constructor
s2 = Sentence "Julie" "loves" "dogs"
