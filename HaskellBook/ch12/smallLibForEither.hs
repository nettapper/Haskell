-- use foldr eventually
lefts' :: [Either r l] -> [r]
lefts' es = foldr f [] es
  where f (Right _) list = list
        f (Left r) list = r : list

-- use foldr eventually
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right r) list = r : list
        f (Left _) list = list

-- Needed to make GhcMod work!
main :: IO()
main = return ()
