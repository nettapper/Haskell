-- You can test your self on the types of partially applying arguments w/o defining the functions
--
-- let f :: a -> a -> a -> a; f = undefined
-- let x :: Char; x = undefined
-- :t f x

f :: a -> a -> a -> a
f = undefined
x :: Char
x = undefined
-- :t f x

g :: a -> b -> c -> b
g = undefined
-- :t g 0 'c' "woot"

h :: (Num a, Num b) => a -> b -> b
h = undefined
-- :t h 1.0 2
-- :t h 1 (5.5 :: Double)

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
-- :t jackal "keyboard" "has the word jackal in it"
-- :t jackal "keyboard"

kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
-- :t kessel 1 2
-- :t kessel 1 (2 :: Integer)
-- :t kessel (1 :: Integer) 2
