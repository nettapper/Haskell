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
  then putStrLn "treeMap okay!"
  else error "treeMap failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder tree = preorder' tree []
  where preorder' Leaf list                = list
        preorder' (Node left a right) list = [a] ++ (preorder' left list) ++ (preorder' right list)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder tree = inorder' tree []
  where inorder' Leaf list                = list
        inorder' (Node left a right) list = (inorder' left list) ++ [a] ++ (inorder' right list)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder tree = postorder' tree []
  where postorder' Leaf list                = list
        postorder' (Node left a right) list =(postorder' left list) ++ (postorder' right list) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
