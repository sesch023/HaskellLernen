-- Higher Order Functions
app :: (a -> b) -> a -> b
app f x = f x

add1 :: Int -> Int
add1 x = x + 1

-- Anonymous Functions
add1_anno :: Int -> Int
add1_anno = \x -> x + 1

add_three = \x y z -> x + y + z

-- Higher Order + Annonymous
x = app (\x -> x + 1) 1

-- Map -> Applies Function to List of Arguments
y = _ (\(x, y) -> x + y) [(1,2), (2,3), (3,4)]

-- Filter -> Filters List by Condition, if true -> in
z = filter (\x -> x `mod` 2 == 0) [1,2,3,4,5,6]