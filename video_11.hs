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