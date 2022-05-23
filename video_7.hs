-- Partial Function Application
add :: Int -> Int -> Int
add = (\x -> (\y -> x+y))
x = add 1

doubleList = map (\x -> 2*x)