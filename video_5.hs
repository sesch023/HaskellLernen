-- Is Element in List?
elem_in :: (Eq a) => a -> [a] -> Bool
elem_in _ [] = False
elem_in v (x:xs) 
 | x == v = True
 | otherwise = elem_in v xs

-- Remove Double Elements
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub x = aux [] x
 where 
  aux y [] = y
  aux y (x:xs)
   | elem_in x y = aux y xs
   | otherwise = aux (y ++ [x]) xs

-- Nub better
nub_better :: (Eq a) => [a] -> [a]
nub_better [] = []
nub_better (x:xs)
 | elem_in x xs = nub xs
 | otherwise = x : nub xs

-- Is List ascending?
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc (x:xs) 
 | length xs == 0 = True
 | x <= head xs = isAsc xs
 | x > head xs = False

-- Is there a path?
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = False
hasPath x y z = if y == z then True else aux x y z [] where
 aux x y z visited
  | y == z = True
  | length (take_starts x y) == 0 = False
  | elem_in y visited = False
  | otherwise = or(map helper (take_starts x y)) where
     helper new_start = aux x (snd new_start) z (visited ++ [y])


take_starts :: [(Int, Int)] -> Int -> [(Int, Int)]
take_starts [] _ = []
take_starts (x:xs) y
 | fst x == y = x : take_starts xs y
 | otherwise = take_starts xs y