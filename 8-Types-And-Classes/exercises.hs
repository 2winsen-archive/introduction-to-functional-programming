import Prelude hiding (GHC.Base)

-- 1
data Nat = Zero | Succ Nat deriving Show

add2 :: Nat -> Nat -> Nat
add2 Zero n = n
add2 (Succ m) n = Succ (add2 m n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = add2 (mult m n) n


-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occursBinaryTree :: Ord a => a -> Tree a -> Bool
occursBinaryTree x (Leaf y) = x == y
occursBinaryTree x (Node l y r) = case compare x y of
                                    GT -> occursBinaryTree x r
                                    LT -> occursBinaryTree x l
                                    EQ -> True


-- 3 
data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a)
t3 = Node3 (Node3 (Node3 (Node3 (Leaf3 0) (Leaf3 1)) (Leaf3 2)) (Leaf3 3)) (Node3 (Leaf3 4) (Node3 (Leaf3 5) (Leaf3 6)))

leaves :: Tree3 a -> Int
leaves (Leaf3 _) = 1
leaves (Node3 l r) = leaves l + leaves r

balanced :: Tree3 a -> Bool
balanced (Leaf3 _) = True
balanced (Node3 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r


-- 4
balance :: [a] -> Tree3 a
balance [] = error "Empty trees are not supported"
balance [x] = Leaf3 x
balance xs = Node3 (balance h1) (balance h2)
    where (h1, h2) = splitInHalf xs

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([], [])
splitInHalf xs = (take half xs, drop half xs)
    where half = length xs `div` 2    


-- 5
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val x) = x
value (Add x y) = value x + value y    

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add x y) = g (folde f g x) (folde f g y)

foldeT1 = Add (Val 2) (Val 3)
foldeT2 = Add (Add (Val 2) (Val 3)) (Val 4)


-- 6
eval :: Expr -> Int
eval = folde (id) (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)


-- 7
data Color a = Red a | Green a | Blue a
instance Eq a => Eq (Color a) where    
    Red a == Red b = a == b
    Green a == Green b = a == b
    Blue a == Blue b = a == b
    _ == _ = False