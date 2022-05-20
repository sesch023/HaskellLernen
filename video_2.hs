-- Video 1

main = putStrLn "hello world!"

-- Video 2

-- Range Funktion, >=, <= und && sind infix
in_range :: Integer -> Integer -> Integer -> Bool
in_range min max x = x >= min && x <= max

-- Mit Let Binding:
in_range_let :: Integer -> Integer -> Integer -> Bool
in_range_let min max x =
    let in_lower_bound = min <= x
        in_upper_bound = max >= x
    in in_lower_bound && in_upper_bound

-- Mit Where Binding:
in_range_where :: Integer -> Integer -> Integer -> Bool
in_range_where min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

