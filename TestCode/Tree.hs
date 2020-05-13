data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) = [a] ++ (preOrder left) ++ (preOrder right)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) = (inOrder left) ++ [a] ++ (inOrder right)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) = (postOrder left) ++ (postOrder right) ++ [a]

foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree _ acc Leaf = acc
foldrTree function acc tree = foldr function acc (inOrder tree)
