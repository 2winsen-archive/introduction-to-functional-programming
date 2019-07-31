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