import Data.Char

hw = putStrLn "Hello World"

greet :: IO()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    let uname = map toUpper name
    putStrLn ("Hello " ++ uname ++ ".")

main :: IO()
main = do
    i <- getLine
    if i /= "quit" then do
        putStrLn ("Input: " ++ i)
        main
    else
        return ()