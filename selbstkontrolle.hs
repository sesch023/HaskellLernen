-- Lambda Ausdrücke

f = \x -> x + 1
g = \x y -> sqrt(x^2+y^2)
apply = \f x -> f x
multiapply = \f x n -> if n == 0 then x else multiapply f (f x) (n-1)

-- Folding

double x = 2*x
inc x = x+1
dec x = x-1
halb_int x = x `div` 2
list = [double, inc, halb_int, dec]

compose funcs x = foldl (.) id (reverse funcs) x

-- Reverse
-- [] ++ [3] -> [3]
-- [3] ++ [2] -> [3, 2]
-- [3, 2] ++ [1] -> [3, 2, 1]
