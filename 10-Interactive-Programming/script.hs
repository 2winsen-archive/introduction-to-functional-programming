import System.IO

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine2 :: IO String
getLine2 = do 
    x <- getChar
    if x == '\n' then
        return []
    else 
        do 
            xs <- getLine2
            return (x:xs)

getCh :: IO Char
getCh = do 
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

sgetLine :: IO String
sgetLine = do 
    x <- getCh
    if x == '\n' then
        do 
            putChar x
            return []
    else 
        do 
            putChar '-'
            xs <- sgetLine
            return (x:xs)

putStr2 :: String -> IO ()
putStr2 [] = return ();
putStr2 (x:xs) = do putChar x
                    putStr2 xs

putStrLn2 :: String -> IO ()
putStrLn2 [] = return ();
putStrLn2 xs = do putStr2 xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStrLn2 "Enter string"
            xs <- getLine2
            putStr2 ("The string " ++ xs ++ " has ")
            putStr2 (show (length xs))
            putStrLn2 " characters"

-- Exercise 1
putStr1 :: String -> IO ()
putStr1 s = sequence_ [putChar c | c <- s]

-- Exercise 4
adder4 :: IO ()
adder4 = do
    putStr "How many numbers? "
    n <- getLine
    ns <- getNumbers4 (read n)
    putStrLn ("The total is " ++ show (sum ns))

getNumbers4 :: Int -> IO [Int]
getNumbers4 n 
    | n <= 0 = return []
    | otherwise = do
        num <- getLine
        do 
            nums <- getNumbers4 (n-1)
            return ((read num::Int):nums)

-- Exercise 5
adder5 :: IO ()
adder5 = do
    putStr "How many numbers? "
    n <- getLine2
    ns <- getNumbers5 (read n)
    putStrLn ("The total is " ++ show (sum ns))

getNumbers5 :: Int -> IO [Int]
getNumbers5 n
    | n <= 0 = return []
    | otherwise = sequence [do 
        num <- getLine2
        return (read num::Int) | x <- [1..n]]
    
-- Exercise 6
readLine :: IO String   
readLine = do
    l <- readCharacters
    return (deleteCharacters 0 (reverse l))   

readCharacters :: IO String
readCharacters = do
    c <- getCh
    if c == '\n' then
        do 
            putChar c
            return []
    else     
        do           
            putChar (if c == '\DEL' then '\b' else c)
            cs <- readCharacters 
            return (c:cs)

deleteCharacters :: Int -> String -> String
deleteCharacters n "" = ""
deleteCharacters n (x:xs) 
    | x == '\DEL' = deleteCharacters (n+1) xs
    | n > 0 = deleteCharacters (n-1) xs
    | otherwise = (deleteCharacters 0 xs) ++ [x]
