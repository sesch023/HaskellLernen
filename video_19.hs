-- Infinite Lists
-- Fibonacci
fibonacci :: Double -> [Double]
fibonacci 0 = [0]
fibonacci 1 = [1, 0]
fibonacci x = (fib!!0 + fib!!1) : fib 
    where
        fib = (fibonacci (x - 1))

-- Better
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)