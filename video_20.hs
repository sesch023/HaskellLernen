import System.IO  

-- Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving(Show)

inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = inv_tup_tree_rec (0, 0) 

inv_tup_tree_rec :: (Integer, Integer) -> Tree (Integer, Integer)
inv_tup_tree_rec (x, y) = Node (inv_tup_tree_rec (x + 1, y)) (x, y) (inv_tup_tree_rec (x, y + 1))

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut max Leaf = Leaf
cut max (Node a b c) = Node (cut (max - 1) a) b (cut (max - 1) c)

-- Exercise 2
test_tree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node Leaf 6 (Node Leaf 7 Leaf))

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node a b c) 
    | b > x = Node (insert x a) b c
    | b < x = Node a b (insert x c)
    | otherwise = Node a b c

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node a b c) = (inorder a) ++ [b] ++ (inorder c)

-- Exercise 3