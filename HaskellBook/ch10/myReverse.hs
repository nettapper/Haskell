-- just def'ning my own rev funcs

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
revl = foldl (\a b -> b ++ [a]) []

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
revr = foldr (\a b -> b ++ [a]) []

-- the difference between foldl and foldr is apparent
