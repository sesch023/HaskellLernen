import System.IO  

file = "input"

split_string :: [Char] -> Char -> [[Char]]
split_string "" x = []
split_string s spl = foldr (\x acc -> if x == spl then [] : acc else (x : head acc) : (tail acc)) [[]] s

aoc_1_1 :: [Int] -> Int
aoc_1_1 [] = 0
aoc_1_1 (x:xs)
 | length xs == 0 = 0
 | x < head xs = 1 + aoc_1_1 xs
 | x >= head xs = aoc_1_1 xs
 | otherwise = 0

aoc_1_2 :: [Int] -> Int
aoc_1_2 (a:b:c:d:xs)
 | (a + b + c) < (b + c + d) = 1 + if (length xs > 0) then aoc_1_2 (b:c:d:xs) else 0
 | (a + b + c) >= (b + c + d) = if (length xs > 0) then aoc_1_2 (b:c:d:xs) else 0

main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    putStr $ show $ aoc_1_1 $ map (\x -> read x :: Int) $ split_string contents '\n' 
    putStr "\n"
    putStr $ show $ aoc_1_2 $ map (\x -> read x :: Int) $ split_string contents '\n' 
    putStr "\n"
    hClose handle
