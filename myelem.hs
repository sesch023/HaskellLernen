myelem :: (Eq a) => a -> [a] -> Bool

myelem x [] = False
myelem x (y:ys)
 | x == y = True
 | otherwise = myelem x ys 
