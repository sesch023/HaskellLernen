sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]
