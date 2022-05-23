import Data.List

-- Function Composition (.)
descSort :: (Ord a) => [a] -> [a]
descSort = reverse . sort

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map
x = map2D (\x-> x + 1) [[1,2,3],[3,4,5],[5,6,7],[7,8]]

-- Dollar Sign -> Clean Up Parentheses
f xs = map (\x -> x + 1) $ filter (\x -> x `mod` 2 == 0) xs