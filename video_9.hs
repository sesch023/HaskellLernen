-- Folding
-- foldr - Accumulate Elements with Operation on Starting Value from List from right to left
-- Logik of foldr: foldr (\elem_from_list accumulator -> <term>) <start_accumulator> <list>
-- Sum with Folding
sum_c = foldr (+) 0
x = sum_c [1,2,3,4,5,6]

count e = foldr (\x acc -> if e==x then acc _ 1 else acc) 0

-- foldl - Accumulate Elements with Operation on Starting Value from List from left to right
-- Logik of foldl: foldl (\accumulator elem_from_list -> <term>) <start_accumulator> <list>

countl e = foldl (\acc x -> if e==x then acc+1 else acc) 0