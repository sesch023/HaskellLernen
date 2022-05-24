-- Ex 1:
rev :: [a] -> [a]
rev = foldr (:) []

-- Ex 2:
hlp [] x = [[x]]
hlp acc x = acc ++ [last acc ++ [x]]

prefixes :: [a] -> [[a]]
prefixes = foldl hlp []


-- Ex 3:
lagrange_poly :: [(Float, Float)] -> Float -> Int -> Float
lagrange_poly data_x x j = fst (foldl (\acc xi -> (if (snd acc) == j then (fst acc) else ((fst acc) * (x - (fst xi))) / ((fst (data_x!!j)) - (fst xi)), (snd acc) + 1)) (1, 0) data_x)

lagrange :: [(Float, Float)] -> Float -> Float
lagrange data_x x = fst (foldl (\acc xj -> ((fst acc) + (snd xj) * (lagrange_poly data_x x (snd acc)), (snd acc) + 1)) (0,0) data_x)

-- Ex 4: uff
data Trie a = Leaf a | Node a [Trie a]

t = Node 'c' [
 Node 'a'
  [Leaf 'r', Leaf 't'],
 Node 'o'
  [Node 'o'
   [Leaf 'l']]]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf a) = f acc a
foldtrie f acc (Node a x) = foldl f' (f acc a) x
    where
        f' acc t = foldtrie f acc t