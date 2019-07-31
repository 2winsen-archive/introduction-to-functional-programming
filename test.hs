import Data.Char

double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

prod [] = 1
prod (x:xs) = x * prod xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

rqsort xs = reverse (qsort xs)

rqsort2 [] = []
rqsort2 (x:xs) = rqsort2 larger ++ [x] ++ rqsort2 smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last2 xs = head (reverse xs)
last3 xs = xs !! (length xs - 1)

initv1 xs = reverse (drop 1 (reverse xs))
initv2 xs =  reverse (tail (reverse xs))


-- SECTION 4 EXERCISES
halve xs = (take (halfLength xs) xs, drop (halfLength xs) xs)
        where
            halfLength xs = length xs `div` 2

thirdA xs = head (tail (tail xs))
thirdB xs = xs !! 2
thirdC (_:_:x:_) = x

safetailA xs = if null xs then xs else tail xs
safetailB xs 
    | null xs = xs
    | not (null xs) = tail xs
safetailC [] = [] 
safetailC xs = tail xs

lorA True True = True
lorA True False = True
lorA False True = True
lorA False False = False

lorB False False = False
lorB _ _ = True

lorC True _ = True
lorC False a = a

lorD False False = False
lorD _ _ = True

fn45 x y = if x == True then (if y == True then True else False) else False

fn46 x y = if x == True then y else False

fn47 = \x -> \y -> \z -> x * y * z

luhnDouble x
    | x2 > 9 = x2 - 9
    | otherwise = x2
    where x2 = x * 2

luhn x1 x2 x3 x4 = (sum [luhnDouble x1, x2, luhnDouble x3, x4]) `mod` 10 == 0


-- SECTION 5
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


-- SECTION 6
fact6 :: Integer -> Integer
fact6 0 = 1
fact6 n 
    | n >= 0 = n * fact6 (n - 1)
    | otherwise = 0

mult6 :: Integer -> Integer -> Integer
mult6 n 1 = n
mult6 n t = n + mult6 n (t - 1)

replicate6 :: Eq a => Int -> a -> [a]
replicate6 0 x = []
replicate6 n x = x : replicate6 (n-1) x

sel6 :: Eq a => [a] -> Int -> a
sel6 (x:xs) 0 = x
sel6 (x:xs) n = sel6 xs (n-1)

elem6 :: Eq a => a -> [a] -> Bool
elem6 n [] = False
elem6 n [x] = x == n
elem6 n (x:xs) = (x == n) || elem6 n xs

product6 :: [Int] -> Int
product6 [] = 1
product6 (x:xs) = x * product6 xs

length6 :: Eq a => [a] -> Int
length6 [] = 0
length6 (_:xs) = 1 + length6 xs

reverse6 :: Eq a => [a] -> [a]
reverse6 [] = []
reverse6 (x:xs) = (reverse6 xs) ++ [x]

append6 :: Eq a => [a] -> [a] -> [a]
[] `append6` ys = ys
(x:xs) `append6` ys = x : (xs `append6` ys)

insert6 :: Ord a => a -> [a] -> [a]
insert6 x [] = [x]
insert6 x (y:ys)
        | x <= y = x : y : ys
        | otherwise = y : (insert6 x ys)

isort6 :: Ord a => [a] -> [a]
isort6 [] = []
isort6 (x:xs) = insert6 x (isort6 xs)

zip6 :: [a] -> [b] -> [(a, b)]
zip6 _ [] = []
zip6 [] _ = []
zip6 (x:xs) (y:ys) = (x,y) : (zip6 xs ys)

drop6 :: Int -> [a] -> [a]
drop6 0 xs = xs
drop6 _ [] = []
drop6 n (x:xs) = drop (n-1) xs

fib6 :: Int -> Int
fib6 0 = 0
fib6 1 = 1
fib6 n = fib6 (n-1) + fib6 (n-2)

evens6 :: [a] -> [a]
evens6 [] = []
evens6 (x:xs) = x : odds6 xs

odds6 :: [a] -> [a]
odds6 [] = []
odds6 (_:xs) = evens6 xs

-- init6 returns a list without last item
init6 :: [a] -> [a]
init6 [_] = []
init6 (x:xs) = x : init6 xs

-- SECTION 6 EXERCISES
sumdown6 :: Int -> Int
sumdown6 1 = 1
sumdown6 n = n + sumdown6 (n-1)

exp6 :: Int -> Int -> Int
exp6 _ 0 = 1
exp6 0 _ = 0
exp6 n e = n * exp6 n (e-1)

euclid6 :: Int -> Int -> Int
euclid6 x y 
        | x == y = x
        | x > y = euclid6 (x-y) y
        | otherwise = euclid6 x (y-x)

and6 :: [Bool] -> Bool
and6 [] = False
and6 [x] = x
and6 (x:xs) = x && and6 xs

concat6 :: [[a]] -> [a]
concat6 [] = []
concat6 [[]] = []
concat6 (x:xs) = x ++ concat6 xs

merge6 :: Ord a => [a] -> [a] -> [a]
merge6 [] [] = []
merge6 [] ys = ys
merge6 xs [] = xs
merge6 (x:xs) (y:ys)
        | x <= y = merge6 xs (x : y : ys)
        | x > y = y : merge6 (x:xs) ys

havle6 :: [a] -> ([a], [a])
havle6 xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

msort6 :: Ord a => [a] -> [a]
msort6 [] = []
msort6 [x] = [x]
msort6 xs = merge6 (msort6 firstEl) (msort6 secondEl)
        where 
            firstEl = fst (havle6 xs)
            secondEl = snd (havle6 xs)

sum6 :: Num a => [a] -> a
sum6 [] = 0
sum6 (x:xs) = x + sum6 xs

take6 :: Int -> [a] -> [a]
take6 0 _ = []
take6 _ [] = []
take6 n (x:xs) = x : take6 (n-1) xs

last6 :: [a] -> a
last6 [x] = x
last6 (x:xs) = last xs