import Data.Char
import Data.List

length6 :: [a] -> Int
length6 xs = foldr (\_ acc -> acc + 1) 0 xs

length6' :: [a] -> Int
length6' xs = foldl (\acc _ -> acc + 1) 0 xs

map6 :: (a -> a) -> [a] -> [a]
map6 f xs = foldr (\curr acc -> f(curr) : acc) [] xs

reverse6 :: [a] -> [a]
reverse6 xs = foldr (\curr acc -> acc ++ [curr]) [] xs

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

iterate' :: (a -> a) -> a -> [a]
iterate' f z = foldr (\_ acc -> if (length acc == 0) then z : acc else acc ++ [f (last acc)] ) [] [1..10]


-- BINARY STRING TRANSMITTER
type Bit = Int

--1011 = 13
--1248
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\curr acc -> curr + 2*acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


-- VOTING ALGORITHMS
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result 

-- ALTERNATIVE
ballots :: [[String]]
ballots = [["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim c = map (filter (/= c))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
    [c] -> c
    (c:cs) -> winner' (elim c bs)

-- EXERCISES
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

all6 :: (a -> Bool) -> [a] -> Bool
all6 p = and . map p

any6 :: (a -> Bool) -> [a] -> Bool
any6 p = or . map p

takeWhile6 :: (a -> Bool) -> [a] -> [a]
takeWhile6 p [] = []
takeWhile6 p (x:xs) = if (p x) then x : takeWhile6 p xs else []

dropWhile6 :: (a -> Bool) -> [a] -> [a]
dropWhile6 p [] = []
dropWhile6 p (x:xs) = if (p x) then dropWhile6 p xs else x:xs

filter6 :: (a -> Bool) -> [a] -> [a]
filter6 p xs = foldr (\curr acc -> if (p curr) then curr:acc else acc) [] xs

dec2int :: [Int] -> Int
dec2int xs = foldr (\curr acc -> (fst curr) * (snd curr) + acc) 0 (zip (reverse xs) (iterate (*10) 1))

dec2int2 :: [Int] -> Int
dec2int2 = foldl (\acc curr -> 10 * acc + curr) 0

curry6 :: ((a,b) -> c) -> a -> b -> c
curry6 f = \x y -> f (x, y)

uncurry6 :: (a -> b -> c) -> (a, b) -> c
uncurry6 f = \(x, y) -> f x  y

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

--    int2bin :: Int -> [Bit]
--    int2bin 0 = []
--    int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

mapUnfold f = unfold (== []) (f . head) (tail)

iterateUnfold f = unfold (const False) id f

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = map fn pairs
    where pairs = zip xs [0..]
          fn (x, n) | even n = f1 x
                    | otherwise = f2 x

                    