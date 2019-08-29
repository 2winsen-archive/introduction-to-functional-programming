import Data.Char

factors n = [x | x <- [1..n], n `mod` x == 0]
prime n = (factors n) == [1, n]
primes n = [x | x <- [2..n], prime x]

pairs xs = zip xs (tail xs)
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions x xs = [i | (x', i) <- zip xs indices, x == x']
    where indices = [0..length xs - 1]

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- Caesar cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` lettersLength)
          | otherwise = c
          where lettersLength = 26

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Cracking
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n  | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Num a => Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where 
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- SECTION 5 EXERCISES
sumOfSquares :: Int
sumOfSquares = sum [n'^2 | n' <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x, y) <- grid n n, x /= y]

replicate2 :: Ord a => Int -> a -> [a]
replicate2 n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | 
    x <- [1..n], 
    y <- [1..n], 
    z <- [1..n], 
    x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
        where isPerfect x = (sum (factors x)) - x == x

fn57 = [(x, y) | x <- [1,2], y <- [3,4]]
fn57' = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs indices)
    where indices = [0..length xs - 1]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]