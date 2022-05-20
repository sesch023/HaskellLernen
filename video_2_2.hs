in_range :: Integer -> Integer -> Integer -> Bool
in_range min max x = upper && lower
 where
  upper = max <= x
  lower = min >= x
