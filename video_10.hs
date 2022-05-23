data Color = 
    Red | Orange | Yellow | Green | Blue | Magenta

data Calculation =
    Add Int Int | Sub Int Int | Mul Int Int | Div Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mul x y) = x * y
calc (Div x y) = div x y

-- Pea Num
data PeaNum = Succ PeaNum | Zero

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum
decr (Succ n) = n

add :: PeaNum -> PeaNum -> PeaNum
add Zero n = n
add (Succ m) n = Succ $ add m n

sump :: [PeaNum] -> PeaNum
sump [] = Zero
sump (x:xs) = add x $ sump xs

-- Liste
data Liste a = El a (Liste a) | ZeroL 
liste = El 5 $ El 6 $ El 7 $ ZeroL

-- Tree
data Tree a = Leaf | Node (Tree a) a (Tree a)
tree = Node (Node (Node Leaf 5 Leaf) 7 Leaf) 10 (Node (Node Leaf 3 Leaf) 4 Leaf)