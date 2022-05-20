-- List Constructors

-- x = []
-- y = x:xs

-- Elements between n and m as list

asc :: Int -> Int -> [Int]
asc n m 
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n+1) m

-- Functions on Lists

first_element = head (asc 0 10)
everything_but_first = tail (asc 0 10)
length_of_list = length (asc 0 100)
last_removed = init (asc 0 100)
list_empty = null []
list_not_empty = null (asc 0 10)
and_list = and [True, False, True]
or_list = or [True, False, True]

-- List comprehension
times_two = [2 * x | x <- asc 0 10]
-- With Guard
times_two_if_even = [2 * x | x <- asc 0 10, mod x 2 == 0]
-- With Multiple List
combined = [(x, y) | x <- asc 0 10, y <- asc 10 20]
-- With Multiple Lists and Guards
combined_only_even = [(x, y) | x <- asc 0 10, y <- asc 10 20, mod x 2 == 0, mod y 2 == 0]

-- List Patterns

sum_ele :: [Int] -> Int
sum_ele [] = 0
sum_ele (x:xs) = x + sum_ele xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) 
    | mod x 2 == 0 = x : evens xs
    | otherwise = evens xs

-- Tuples
first = fst (1, 2)
second = snd (1, 2)
-- Tuples with Bindings
take_x = let (x, y) = (1,2) in x

add_tuples :: [(Int, Int)] -> [Int]
add_tuples xs = [ (x + y) | (x, y) <- xs]