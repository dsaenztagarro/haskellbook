module MapForBinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' a left) b right
  | b > a  = Node left a (insert' b right)
insert' _ (Node _ _ _) = Leaf -- unknown situation

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left b right) = b : ((preorder left) ++ (preorder right))

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left b right) = (inorder left) ++ [b] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left b right) = (postorder right) ++ (postorder left) ++ [b]

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
  if postorder testTree == [3, 1, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right