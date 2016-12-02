-- data MyList = Null | Cons a MyList  -- Not in scope type var 'a'
-- data MyList a = Null | Cons a MyList  -- Don't forget to pass 'a' to the type constructor
-- data MyList a = Null | Cons a (MyList a)
data MyList hi = Null | Cons hi (MyList hi)  -- remember that names of vars don't matter
  deriving Show

-- To see the kind of MyList
-- :k MyList
-- MyList * -> *

-- :t Null
-- Null :: MyList hi

-- :t Cons
-- Cons :: hi -> MyList hi -> MyList hi
