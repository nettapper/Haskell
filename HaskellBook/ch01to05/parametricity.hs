-- Given the type a -> a, which is the type for id, attempt to make a function that is not bottom and terminates successfully that does something other than returning the same value.
myId :: a -> a
myId a = a

-- or I don't know why but this works
-- myId a = id a

-- and this one won't teminate
-- myId a = myId $ myId a


-- This hypothetical function a -> a -> a has two–and only two–implementations. Write both possi- ble versions of a -> a -> a. A er doing so, try to violate the constraints of parametrically polymorphic values we outlined above.
one :: a -> a -> a
one a _ = a

two :: a -> a -> a
two _ a = a

-- Implement a -> b -> b. How many implementations can it have? Does the behavior change when the types of 𝑎 and 𝑏 change?
f :: a -> b -> b
f _ b = b  -- I can only think of one
