quicksort :: [Int] -> [Int]
quicksort x 
    | length x <= 1 = x
    | otherwise = quicksort(lower) ++ equal ++ quicksort(higher)
        where
            lower = [ z | z <- x, z < head(x)]
            higher = [ z | z <- x, z > head(x)]
            equal = [ z | z <- x, z == head(x)]

-- Quick sort in Haskell
quicksort_r :: (Ord a) => [a] -> [a]
quicksort_r [] = []
quicksort_r (x:xs) =
    let smallerSorted = quicksort_r [a | a <- xs, a <= x]
        biggerSorted = quicksort_r [a | a <- xs, a > x]
    in  smallerSorted ++ [x] _ biggerSorted
