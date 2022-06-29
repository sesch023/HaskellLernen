data Tree a = Leaf | Node (Tree a) a Int (Tree a)
    deriving (Show, Eq)

-- instance (Show a) => Show (Tree a) where
--    show Leaf = ""
--    show (Node Leaf x Leaf) = show x
--    show (Node Leaf y z) = show y ++ "(" ++ show z ++ ")"
--    show (Node x y Leaf) = "(" ++ show x ++ ")" ++ show y
--    show (Node x y z) = "(" ++ show x ++ ")" ++ show y ++ "(" ++ show z ++ ")"

-- instance Eq(a) => Eq (Tree a) where
--    Leaf == Leaf = True
--    (Node x y z) == (Node a b c) = (y == b && x == a && z == c)
--    _ == _ = False

test_tree = Node (Node Leaf 5 1 Leaf) 10 1 (Node (Node Leaf 12 1 (Node Leaf 15 1 (Node Leaf 18 1 Leaf))) 20 2 Leaf)
test_tree_2 = Node (Node Leaf 5 1 Leaf) 10 1 (Node (Node Leaf 12 1(Node Leaf 15 1 (Node Leaf 18 1 Leaf))) 20 2 Leaf)
test_tree_3 = Node (Node Leaf 2 1 Leaf) 10 1 (Node (Node Leaf 12 1 (Node Leaf 15 1 (Node Leaf 18 1 Leaf))) 20 2 Leaf)

test_list = [10, 5, 2, 6, 15, 20, 17, 13, 5, 13]

buildTree :: Ord(a) => [a] -> Tree a
buildTree values = foldl (\acc x -> insertToTree x acc) Leaf values

inOrderTree :: Tree a -> [a]
inOrderTree Leaf = []
inOrderTree (Node a b c d) = (inOrderTree a) ++ (take c (repeat b)) ++ (inOrderTree d)

insertToTree :: Ord(a) => a -> Tree a -> Tree a
insertToTree x Leaf = Node Leaf x 1 Leaf
insertToTree x (Node a b c d) 
    | b > x = (Node (insertToTree x a) b c d) 
    | b < x = (Node a b c (insertToTree x d)) 
    | b == x = (Node a b (c + 1) d) 

removeFromTree :: Ord(a) => a -> Tree a -> Tree a
removeFromTree a b = foldr insertToTree tree removed
    where
        (tree, removed) = removeFromTreeRec a b

removeFromTreeRec :: Ord(a) => a -> Tree a -> (Tree a, [a])
removeFromTreeRec x (Node Leaf a 1 Leaf) = (Leaf, [])
removeFromTreeRec x (Node a b c d)
    | b > x = let (tree, acc) = removeFromTreeRec x a in (Node tree b c d, acc)
    | b < x = let (tree, acc) = removeFromTreeRec x d in (Node a b c tree, acc)
    | b == x && c > 1 = (Node a b (c - 1) d, [])
    | otherwise = let
        acc = (inOrderTree a) ++ (inOrderTree d)
      in (Leaf, acc)

    