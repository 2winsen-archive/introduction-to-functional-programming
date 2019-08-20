module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens = filter even

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer]
squares n = map (^2) [1..n]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Integer -> Integer -> [Integer]
squares' m n = map (^2) (map fst (zip [n+1..] [1..m]))

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

-- coords 1 1 = [(0,0), (0,1), (1,0), (1,1)] 
-- coords 1 2 = [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
coords :: Integer -> Integer -> [(Integer,Integer)]
coords x y = [(x',y') | x' <- [0..x], y' <- [0..y]]
