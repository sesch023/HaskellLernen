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

most_common_at_index :: [[Int]] -> Int -> Int -> Int
most_common_at_index x index equal
    | common > half_length = 1
    | common == half_length = equal
    | common < half_length = 0
    where 
        common = fromIntegral $ foldl (\acc x -> acc + (x!!index)) 0 x
        half_length = fromIntegral (length x) / 2.0

check_bit_numbers :: [[Int]] -> ([Int], Int)
check_bit_numbers x = foldl (\(list, index) value -> (zipWith (+) list value, index + 1)) (create_base_list $ head x, 0) x

take_most_common_at_index_keep_1 :: [[Int]] -> Int -> [[Int]]
take_most_common_at_index_keep_1 x index = filter (\x -> take_index == (x!!index)) x
    where 
        take_index = most_common_at_index x index 1

take_least_common_at_index_keep_0 :: [[Int]] -> Int -> [[Int]]
take_least_common_at_index_keep_0 x index = filter (\x -> take_index /= (x!!index)) x
    where 
        take_index = most_common_at_index x index 1

aoc_3_2_pass :: ([[Int]] -> Int -> [[Int]]) -> [[Int]] -> Int -> Int
aoc_3_2_pass _ [] _ = 0
aoc_3_2_pass _ [x] _ = binary_int_list_to_int x
aoc_3_2_pass f x index = (aoc_3_2_pass f (f x index) (index + 1))

aoc_3_2 :: [[Int]] -> (Int, Int)
aoc_3_2 x = (oxy, co2)
    where
        oxy = aoc_3_2_pass (take_most_common_at_index_keep_1) x 0 
        co2 = aoc_3_2_pass (take_least_common_at_index_keep_0) x 0 

main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    putStr $ show  $ take_most_common_at_index_keep_1 (map binary_string_to_int_array $ split_string contents '\n') 0
    putStr "\n"
    putStr $ show $ check_bit_numbers $ map binary_string_to_int_array $ split_string contents '\n'
    putStr "\n"
    putStr $ show $ aoc_3_2 $ map binary_string_to_int_array $ split_string contents '\n'
    putStr "\n"
    hClose handle