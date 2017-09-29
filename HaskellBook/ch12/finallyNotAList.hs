-- Needed to make GhcMod work!
main :: IO()
main = do
  test
  return ()

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq, Ord)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f a = case f a of
                  Just (a0,b,a1) -> Node (unfold f a0) b (unfold f a1)
                  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0
  where f :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
        f n x = if x < n
                 then Just (x + 1, x, x + 1)
                 else Nothing

test :: IO()
test = do
  putStrLn $ show $ Leaf == treeBuild 0
  putStrLn $ show $ (Node Leaf 0 Leaf) == treeBuild 1
  let x = Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
  putStrLn $ show $ x == treeBuild 2

