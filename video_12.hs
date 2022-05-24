-- Records
data Person = Person { name :: String, age :: Int }

pers = Person "Sebastian" 25
greet :: Person -> [Char]
greet x = "Hi " ++ name x

say_age :: Person -> [Char]
say_age x = "My age is " ++ show (age x)

data Point = 
    D2 { x :: Int, y :: Int}
  | D3 { x :: Int, y :: Int, z :: Int }