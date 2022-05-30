import System.IO  

file = "input"

split_string :: [Char] -> Char -> [[Char]]
split_string "" x = []
split_string s spl = foldr (\x acc -> if x == spl then [] : acc else (x : head acc) : (tail acc)) [[]] s

clean_input :: [[Char]] -> [[Char]]
clean_input x = filter (\x -> x /= "") x

group :: Int -> [a] -> [[a]]
group _ [] = []
group x y 
    | length y > x = take x y : (group x (drop x y))
    | otherwise = [y]

get_boards :: [[Char]] -> [[[(Int, Bool)]]]
get_boards x = group 5 $ map (\x -> map (\y -> (read y :: Int, False)) $ clean_input $ split_string x ' ') x


main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    let file_data = clean_input $ split_string contents '\n' 
    let numbers = map (\x -> read x :: Int) $ clean_input $ split_string (head file_data) ','
    let boards = get_boards $ tail file_data
    putStrLn $ show $ boards
    hClose handle