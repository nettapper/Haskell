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

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right r) = Just $ f r
eitherMaybe' _ (Left _) = Nothing

either'::(a -> c) -> (b -> c) -> Either a b -> c
either' fac fbc (Left a) = fac a
either' fac fbc (Right b) = fbc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' fbc (Right b) = Just $ fbc b


-- Needed to make GhcMod work!
main :: IO()
main = return ()
