import System.IO  
import Data.Char (toUpper)

file = "input"

data Direction
    = Forward
    | Down
    | Up
    deriving (Show, Read)

data Command = Command { name :: Direction, value :: Int } 
    deriving (Show)
data Position = Position { horizontal :: Int, depth :: Int, aim :: Int }
    deriving (Show)

split_string :: [Char] -> Char -> [[Char]]
split_string "" x = []
split_string s spl = foldr (\x acc -> if x == spl then [] : acc else (x : head acc) : (tail acc)) [[]] s

aoc2_2_raw_to_tuple :: [[Char]] -> Command
--aoc2_2_raw_to_tuple ("forward":y:xs) = Command Forward (read y :: Int)
--aoc2_2_raw_to_tuple ("down":y:xs) = Command Down (read y :: Int)
--aoc2_2_raw_to_tuple ("up":y:xs) = Command Up (read y :: Int)

aoc2_2_raw_to_tuple (x:y:xs) = Command (read . cap $ x) (read y) 
    where 
        cap []       = "" 
        cap (x : xs) = toUpper x : xs


aoc2_2 :: [Command] -> Int
aoc2_2 x = (horizontal pos) * (depth pos)
    where 
        pos = foldl (\acc x -> aoc2_2_step x acc) (Position 0 0 0) x

aoc2_2_step :: Command -> Position -> Position
aoc2_2_step (Command Forward command) (Position x y z) = (Position (x + command) (y + z * command) z)
aoc2_2_step (Command Down command) (Position x y z) = (Position x y (z + command))
aoc2_2_step (Command Up command) (Position x y z) = (Position x y (z - command))

main = do  
    handle <- openFile file ReadMode  
    contents <- hGetContents handle 
    putStr $ show $ aoc2_2 $ map (aoc2_2_raw_to_tuple . words) $ split_string contents '\n' 
    putStr "\n"
    hClose handle
