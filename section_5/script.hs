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
msort6 xs = merge6 (msort6 as) (msort6 bs)
        where (as, bs) = havle6 xs

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