-- Now given a type write this funciton

-- only one that doesn't go into an infinite loop
i :: a -> a
i a = a

-- only one
c :: a -> b -> a
c a _ = a

-- given alpha equivilence are c an c'' the same?
c'' :: b -> a -> b
c'' = c -- yes

-- only one works
c' :: a -> b -> b
c' _ b = b

-- multiple, you've seen atleast two before
r :: [a] -> [a]
r xs = xs

r' :: [a] -> [a]
r' (_:xs) = xs
-- others include init, reverse, cycle, safetail

-- only one version type checks
co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c $ a2b a

-- only one version type checks
a :: (a -> c) -> a -> a
a _ a = a

-- only one version will type check
a' :: (a -> b) -> a -> b
a' a2b = a2b
-- a' a2b a = a2b a
