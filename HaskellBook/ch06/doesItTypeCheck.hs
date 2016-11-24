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



-- Given a datatype declaration, what can we do?

data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- which will type check?
-- phew = Papu "chases" True  -- Will not

truth = Papu (Rocks "chomskydoz") (Yeah True)  -- Will

equalityForall :: Papu -> Papu -> Bool  -- Will
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool  -- Will not
-- comparePapus p p' = p > p'
