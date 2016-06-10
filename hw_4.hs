data BST a = Leaf | Node (BST a) a (BST a)
 
--skipping exercises 1-12
---------------------------- Exercise 13 ---------------------------------------
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f a Leaf = Node (Leaf) a (Leaf)
insertBST f x bst@(Node l nodeval r)
  | x `f` nodeval == EQ = Node l x r
  | x `f` nodeval == LT = Node (insertBST f x l) nodeval r
  | x `f` nodeval == GT = Node l nodeval (insertBST f x r)

testBST :: (Ord a) => a -> a -> Ordering
testBST x y
  | x < y = LT
  | x > y = GT
  | x == y = EQ
