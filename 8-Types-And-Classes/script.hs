-- Type
type Pos = (Int, Int)
type Trans = Pos -> Pos
goRight :: Trans
goRight p = ((+10) $ fst p, snd p)

type Age = Int
calculateAge :: String -> Age
calculateAge "vitalijs" = 31
calculateAge "lelde" = 29
calculateAge _ = 0

type Pair a = (a,a)
duplicate :: a -> Pair a
duplicate x = (x,x)

type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k xs = head [v' | (k', v') <- xs, k == k']


-- Data
data Move = North | South | East | West deriving Show
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float deriving Show
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2;
area (Rect x y) = x * y;

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)


-- Newtype
-- ensures typesafety w/o affecting performance 
-- as they are automatically being removed by 
-- compiler once type checking is done
newtype Nat1 = N Int
data Nat2 = N2 Int
type Nat3 = Int


-- Recursive types
data Nat = Zero | Succ Nat deriving Show
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)

data List a = Nil | El a (List a) deriving Show
len :: List a -> Int
len Nil = 0
len (El _ xs) = 1 + len xs

arr2List :: [a] -> List a
arr2List [] = Nil
arr2List (x:xs) = El x (arr2List xs)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occursBinaryTree :: Ord a => a -> Tree a -> Bool
occursBinaryTree x (Leaf y) = x == y
occursBinaryTree x (Node l y r) | x > y = occursBinaryTree x r
                                | x < y = occursBinaryTree x l
                                | otherwise = x == y

