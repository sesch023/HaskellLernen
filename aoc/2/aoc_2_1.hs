import System.IO  

file = "input"

data Command = Command { name :: String, value :: Int } 
    deriving (Show)
data Position = Position { horizontal :: Int, depth :: Int }
    deriving (Show)

split_string :: [Char] -> Char -> [[Char]]
split_string "" x = []
split_string s spl = foldr (\x acc -> if x == spl then [] : acc else (x : head acc) : (tail acc)) [[]] s

aoc2_1_raw_to_tuple :: [[Char]] -> Command
aoc2_1_raw_to_tuple (x:y:xs) = Command x (read y :: Int)

aoc2_1 :: [Command] -> Int
aoc2_1 x = (horizontal pos) * (depth pos)
    where 
        pos = foldl (\acc x -> aoc2_1_step x acc) (Position 0 0) x

aoc2_1_step :: Command -> Position -> Position
aoc2_1_step (Command "forward" command) (Position x y) = (Position (x + command) y)
aoc2_1_step (Command "down" command) (Position x y) = (Position x (y + command))
aoc2_1_step (Command "up" command) (Position x y) = (Position x (y - command))

main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    putStr $ show $ aoc2_1 $ map aoc2_1_raw_to_tuple $ map (\x -> split_string x ' ') $ split_string contents '\n' 
    putStr "\n"
    hClose handle
