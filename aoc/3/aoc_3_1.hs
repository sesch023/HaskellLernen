import System.IO  

file = "input"

split_string :: [Char] -> Char -> [[Char]]
split_string "" x = []
split_string s spl = foldr (\x acc -> if x == spl then [] : acc else (x : head acc) : (tail acc)) [[]] s

binary_string_to_int :: [Char] -> Int
binary_string_to_int x = foldl (\acc x -> if x == '1' then acc * 2 + 1 else acc * 2) 0 x

binary_int_list_to_int :: [Int] -> Int
binary_int_list_to_int x = foldl (\acc x -> if x == 1 then acc * 2 + 1 else acc * 2) 0 x

binary_string_to_int_array :: [Char] -> [Int]
binary_string_to_int_array x = foldl (\acc x -> acc ++ [(read [x] :: Int)]) [] x

create_base_list :: [Int] -> [Int]
create_base_list x = replicate (length x) 0

aoc_3_1 :: [[Int]] -> (Int, Int)
aoc_3_1 x = (binary_int_list_to_int $ map (\x -> if x > (div size 2) then 1 else 0) result_list, binary_int_list_to_int $ map (\x -> if x > (div size 2) then 0 else 1) result_list)
    where 
        (result_list, size) = foldl (\(list, index) value -> (zipWith (+) list value, index + 1)) (create_base_list $ head x, 0) x

main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    putStr $ show $ aoc_3_1 $ map binary_string_to_int_array $ split_string contents '\n' 
    putStr "\n"
    putStr $ show $ binary_string_to_int "1010" 
    putStr "\n"
    hClose handle