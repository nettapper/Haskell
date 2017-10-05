type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Show, Eq)

mkPerson :: Name
          -> Age
          -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left $ NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ ". Age was: " ++ show age ++ "."

-- can't modify the above code
-- 1) promt user for name and age (use read for age)
-- 2) construct a person from those
-- 3) if good print yay ++ person val
-- 4) if not print the error that occoured
gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter a name and hit return."
  name <- getLine
  putStrLn "Please enter an age and hit return."
  age <- getLine
  case mkPerson name (read age) of
       (Right p) -> putStrLn $ "Yay! Successfully got a person:" ++ show p
       (Left p) -> putStrLn $ "An error occoured: " ++ show p
  return ()

