import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)
example1 :: Grid
example1 = [[O,B,B],[X,X,O],[X,O,B]]

blank :: Grid -> Bool
blank = all (== B) . concat

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]        
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else 
        do 
            putStrLn "ERROR: Invalid number"
            getNat prompt

tictactoe :: IO ()            
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do 
    cls
    goto (1,1)    
    putGrid g
    run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")    

run' :: Grid -> Player -> IO ()
run' g p 
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | otherwise = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do
                putStrLn "Error: Invalid move"
                run' g p
            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

mindepth :: Tree a -> Int
mindepth (Node g []) = 0
mindepth (Node g ts) = 1 + minimum [mindepth t | t <- ts]

type Depth = Int
adddepths :: Tree a -> Tree (a,Depth)
adddepths node = Node (g, mindepth node) [adddepths t | t <- ts]
    where (Node g ts) = node

moves :: Grid -> Player -> [Grid]
moves g p 
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree (Grid, Depth) -> Tree (Grid, Player, Depth)
minimax (Node (g,d) [])
    | wins O g = Node (g,O,d) []
    | wins X g = Node (g,X,d) []
    | otherwise = Node (g,B,d) []
minimax (Node (g,d) ts)    
    | turn g == O = Node (g, minimum ps, d) ts'
    | turn g == X = Node (g, maximum ps, d) ts'
    where
        ts' = map minimax ts
        ps = [p | Node (_,p,_) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = grid
    where
        (_,grid) = minimumBy (\a b -> compare (fst a) (fst b)) [(d,g') | Node (g',p',d) _ <- ts, p' == best]
        tree = prune depth (adddepths (gametree g p))
        Node (_, best,_) ts = minimax tree        

main :: IO ()
main = do
    cls
    goto (1,1)
    hSetBuffering stdout NoBuffering
    x <- againstai
    case x of
        True -> do
            p <- whoisfirst
            play empty p
        _ -> run empty O

againstai :: IO Bool
againstai = do
    putStr "Against AI (Yes) or Player? (No)? "
    x <- getLine
    case map toLower x of
        "yes" -> return True
        _ -> return False

whoisfirst :: IO Player
whoisfirst = do
    i <- getNat "Do you wish to be first or second? "
    let p = if i > 1 then X else O
    return p

play :: Grid -> Player -> IO ()    
play g p = do
    cls
    goto (1,1)    
    putGrid g
    play' g p

firstmove :: Player -> Grid
firstmove p = [[B,B,B],[B,p,B],[B,B,B]]
    
play' :: Grid -> Player -> IO ()    
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | p == O = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do
                putStrLn "Error: Invalid move"
                play' g p
            [g'] -> play g' (next p)
    | p == X = do
        putStr "Player X is thinking... "
        let m = if blank g then firstmove p else (bestmove g p)
        (play $! m) (next p)