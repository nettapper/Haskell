import Data.Char as C
-- onef = foldr (*) 1 [1..5] == flip (*) 1 [1..5]  -- Erros
one  = foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5]  -- True
one' = foldr (*) 1 [1..5] == foldl (*) 1 [1..5]  -- True

two    = foldl (flip (*)) 1 [1..3]
two'   = (1 * 1) * (foldl (flip (*)) 1 [2..3])
two''  = ((1 * 1) * 2) * (foldl (flip (*)) 1 [3])
two''' = (((1 * 1) * 2) * 3)

-- fix the errors
five   = foldr (++) "" ["woot", "WOOT", "woot"]
five'  = foldr max (C.chr 0) "fear is the little death"
five'' = foldr (&&) True [False, True]

-- will this ever return a diff answer
same = foldr (||) True [False, True]  -- no always true

-- q5 has more parts
e = foldl (flip ((++) . show)) "" [1..5]
f = foldr (const . C.chr) 'a' [1..5]
f' = C.ord $ foldr (const . C.chr) 'a' [1..5]
g = foldr (flip const) 0 "tacos"  -- flipped, now only eval the spine
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]
