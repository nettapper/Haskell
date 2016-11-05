-- p1

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h a = g $ f a

-- p2

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

-- p3

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)


-- p4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = w
  where (w, _) = ywz $ xy x
