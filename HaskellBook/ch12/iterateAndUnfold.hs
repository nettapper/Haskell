
myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                     Just (a, newB) -> [a] ++ myUnfoldr f newB
                     Nothing -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x


-- Needed to make GhcMod work!
main :: IO()
main = return ()
