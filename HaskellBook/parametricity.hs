-- Given the type a -> a, which is the type for id, attempt to make a function that is not bottom and terminates successfully that does something other than returning the same value.
myId :: a -> a
myId a = a

-- or I don't know why but this works
-- myId a = id a

-- and this one won't teminate
-- myId a = myId $ myId a
