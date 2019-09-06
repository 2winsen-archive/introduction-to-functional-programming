import System.IO

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

getCh :: IO Char
getCh = do 
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

maxGuesses = 5

hangman :: IO ()
hangman = do 
    putStrLn "Think of a word:"
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word [] maxGuesses

play :: String -> [String] -> Int -> IO ()
play word allGuesses attempt = do 
    let guessNr = maxGuesses - attempt + 1
    putStr ("Guess Nr " ++ (show guessNr) ++ "? ")                 
    guess <- getLine           
    if guess == word then
        putStrLn "You got it!"
    else
        do
            let allGuessesConcat = concat (guess:allGuesses)
                matched = (match word allGuessesConcat)
            putStrLn matched
            if matched == word then
                putStrLn "You got it!"
            else 
                if (attempt-1) == 0 then 
                    putStrLn "No more guesses. YOU DIED!"
                else
                    play word (guess:allGuesses) (attempt-1)

-- match winter win -> win---
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]