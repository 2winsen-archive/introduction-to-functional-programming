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