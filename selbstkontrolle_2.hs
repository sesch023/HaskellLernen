-- Lambda Ausdrücke
f x = x + 1
fl = \x -> x + 1

f_py x y = sqrt (x^2+y^2)
f_pyl = \x y -> sqrt (x^2+y^2)

apply_f f x = f x
apply_fl = \f x -> f x

multiapply f x n = if n == 0 then x else multiapply f (f x) (n - 1)
multiapplyl = \f x n -> if n == 0 then x else multiapplyl f (f x) (n - 1)

-- Folding
double x = 2*x
inc x = x + 1
dec x = x - 1
halb x = x `div` 2
list = [double, inc, halb, dec]
compose fs x = foldl (\acc f -> f acc) x fs
compose_chain fs x = (foldl (.) id (reverse fs)) x

myrev :: [a] -> [a]
myrev = foldr (\x xs -> xs ++ [x]) []