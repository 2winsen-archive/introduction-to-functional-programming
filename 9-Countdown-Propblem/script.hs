module Main where
    
    data Op = Add | Sub | Mul | Div | Expon
    instance Show Op where
        show Add = "+"
        show Sub = "-"
        show Mul = "*"
        show Div = "/"
        show Expon = "^"

    valid :: Op -> Int -> Int -> Bool
    valid Add _ _ = True
    valid Sub x y = x > y
    valid Mul _ _ = True
    valid Div x y = x `mod` y == 0
    valid Expon x y = False

    maxNum = 1000000

    -- Optimization 2
    validO2 :: Op -> Int -> Int -> Bool
    validO2 Add x y = x <= y && x + y < maxNum
    validO2 Sub x y = x > y
    validO2 Mul x y = x /= 1 && y /= 1 && x <= y && x * y < maxNum
    validO2 Div x y = y /= 1 && x `mod` y == 0
    validO2 Expon x y = x > 1 && y > 1 && y <= 3 && x ^ y < maxNum

    apply :: Op -> Int -> Int -> Int
    apply Add x y = x + y
    apply Sub x y = x - y
    apply Mul x y = x * y
    apply Div x y = x `div` y
    apply Expon x y = x ^ y

    data Expr = Val Int | App Op Expr Expr
    instance Show Expr where
        show (Val n) = show n
        show (App o l r) = brak l ++ show o ++ brak r
            where
                brak (Val n) = show n
                brak e = "(" ++ show e ++ ")"

    values :: Expr -> [Int]
    values (Val n) = [n]
    values (App o l r) = values l ++ values r

    eval :: Expr -> [Int]
    eval (Val n) = [n | n > 0]
    eval (App o l r) = [apply o x y | x <- eval l,
                                      y <- eval r,
                                      valid o x y]

    -- [1,2] = [[], [1], [2], [1,2]]
    subs :: [a] -> [[a]]
    subs [] = [[]]
    subs (x:xs) = yss ++ map (x:) yss
        where yss = subs xs

    -- 3 [1,2] = [[3,1,2], [1,3,2], [1,2,3]]
    interleave :: a -> [a] -> [[a]]
    interleave n [] = [[n]]
    interleave n (x:xs) = [n:x:xs] ++ map (x:) (interleave n xs)

    -- [1,2,3] = [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [x3,1,2], [3,2,1]]
    perms :: [a] -> [[a]]
    perms [] = [[]]
    perms (x:xs) = concat (map (interleave x) (perms xs))

    -- [1,2,3] = [[], [3], [2], [2,3], [3,2], [1], [1,3], [3,1], [1,2], [1,2,3], [2,1,3], 
    --      [2,3,1], [1,3,2], [3,1,2], [3,2,1]]
    choices :: [a] -> [[a]]
    choices = concat . map perms . subs

    solution :: Expr -> [Int] -> Int -> Bool
    solution e ns n = elem (values e) (choices ns) && eval e == [n]

    e :: Expr
    e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

    -- [1,2] = [([1], [2])]
    -- [1,2,3] = [([1], [2,3]), ([1,2], [3])]
    -- [1,2,3,4] = [([1], [2,3,4]), ([1,2], [3,4]), ([1,2,3], [4])]
    split :: [a] -> [([a], [a])]
    split [] = []
    split [_] = []
    split xs = [(take x xs, drop x xs) | x <- [1..n]]
        where n = length xs - 1

    exprs :: [Int] -> [Expr]
    exprs [] = []
    exprs [n] = [Val n]
    exprs ns = [e | (ls, rs) <- split ns,
                    l <- exprs ls,
                    r <- exprs rs,
                    e <- combine l r]

    combine :: Expr -> Expr -> [Expr]
    combine l r = [App o l r | o <- ops]

    ops :: [Op]
    ops = [Add, 
        Sub, 
        Mul, 
        Div,
        Expon]
    

    solutions :: [Int] -> Int -> [Expr]
    solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

    -- Optimization 1
    type Result = (Expr, Int)
    resultsO1 :: [Int] -> [Result]
    resultsO1 [] = []
    resultsO1 [n] = [(Val n, n)]
    resultsO1 ns = [res | (ls, rs)  <- split ns,
                              lx  <- resultsO1 ls,
                              ry  <- resultsO1 rs,
                              res <- combineO1 lx ry]

    resultsO2 :: [Int] -> [Result]
    resultsO2 [] = []
    resultsO2 [n] = [(Val n, n)]
    resultsO2 ns = [res | (ls, rs)  <- split ns,
                              lx  <- resultsO2 ls,
                              ry  <- resultsO2 rs,
                              res <- combineO2 lx ry]

    combineO1 :: Result -> Result -> [Result]                              
    combineO1 (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

    combineO2 :: Result -> Result -> [Result]                              
    combineO2 (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, validO2 o x y]

    solutionsO1 :: [Int] -> Int -> [Expr]
    solutionsO1 ns n = [e | ns' <- choices ns, (e,m) <- resultsO1 ns', m == n]

    solutionsO2 :: [Int] -> Int -> [Expr]
    solutionsO2 ns n = [e | ns' <- choices ns, (e,m) <- resultsO2 ns', m == n]

    main :: IO ()
    main = print (solutionsO2 [1,3,7,10,25,50] 765)


    -- Exercises
    -- 1

    -- [1,2,3] = [[], [3], [2], [2,3], [3,2], [1], [1,3], [3,1], [1,2], [1,2,3], [2,1,3], 
    --      [2,3,1], [1,3,2], [3,1,2], [3,2,1]]
    choicesE :: [a] -> [[a]]
    choicesE xs = [res | xs' <- subs xs, 
                         res <- perms xs']

    -- 2
    removeone :: Eq a => a -> [a] -> [a]
    removeone n [] = [] 
    removeone n (x:xs) | x == n = xs
                       | otherwise = x:removeone n xs

    --- [2,3]
    --- [1,2,3]
    --- True
    isChoice :: Eq a => [a] -> [a] -> Bool
    isChoice [] _ = True
    isChoice cs [] = False
    isChoice (c:cs) xs = elem c xs && isChoice cs (removeone c xs)


    -- 3            
    split' :: [a] -> [([a], [a])]
    split' [] = []
    split' [_] = []
    split' xs = [(take x xs, drop x xs) | x <- [0..n]]
        where n = length xs

    -- 4
    check1 = length . concat . map exprs . choices
    check2 = length . concat . map eval . concat . map exprs . choices

    -- 5
    valid5 :: Op -> Int -> Int -> Bool
    valid5 Add _ _ = True
    valid5 Sub x y = True
    valid5 Mul _ _ = True
    valid5 Div x y = y /= 0 && x `mod` y == 0