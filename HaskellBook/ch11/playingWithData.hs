-- data MyList = Null | Cons a MyList  -- Not in scope type var 'a'
-- data MyList a = Null | Cons a MyList  -- Don't forget to pass 'a' to the type constructor
data MyList a = Null | Cons a (MyList a)
  deriving Show

-- To see the kind of MyList
-- :k MyList
-- MyList * -> *

