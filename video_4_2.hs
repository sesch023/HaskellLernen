import Data.List

create_list_desc :: Int -> [Int]
create_list_desc n
 | n == 0 = []
 | otherwise = n : create_list_desc (n-1)
 
create_list_asc :: Int -> [Int]
create_list_asc n = aux n 1
 where
  aux n m
   | n == 0 = []
   | otherwise = m : aux (n-1) (m+1)

create_range :: Int -> Int -> [Int]
create_range n m
 | n > m = []
 | n == m = [m]
 | otherwise = n : create_range (n+1) m
 
head_x = head [1,3,4,5] -- -> 1
tail_x = tail [1,3,4,5] -- -> [3,4,5]
length_x = length [1,3,4,5] -- -> 4
init_x = init [1,3,4,5] -- -> [1,3,4]
null_x = null [1,3,4,5] -- -> False
and_x = and [True, False, True] -- -> False

-- List Comprehension
lc_1 = [2*x | x <- [1,2,3]]
lc_2 = [2*x | x <- [1,2,3], x > 1]
lc_3 = [ (x, y) | x <- [1,2,3], y <- ['a','b']]

sum_c :: [Int] -> Int
sum_c [] = 0
sum_c (x:xs) = x + sum_c xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
 | mod x 2 == 0 = x : evens xs 
 | otherwise = evens xs
 
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x + y | (x, y) <- xs]

addTuplesTotal :: [(Int, Int)] -> (Int, Int)
addTuplesTotal [] = (0, 0)
addTuplesTotal x = (sum(map fst x), sum(map snd x))