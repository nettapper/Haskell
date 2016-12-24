data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Ord, Eq, Show)

-- Insert will only insert new values, no duplicates are allowed
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert a Leaf                  = Node Leaf a Leaf
insert a (Node left current right) = case a `compare` current of
                                      GT -> Node left current (insert a right)
                                      LT -> Node (insert a left) current right
                                      EQ -> Node left current right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf            = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"
