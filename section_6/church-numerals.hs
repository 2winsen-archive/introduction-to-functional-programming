type Church a = (a -> a) -> a -> a

zero :: Church a
zero = \s z -> z

one :: Church a
one = \s z -> s z

two :: Church a
two = \s -> s . s

three :: Church a
three = \s -> s . s . s

times :: Int -> Church a
times n = \s z -> foldr (\_ acc -> s acc) z [1..n]

c2i :: Church Int -> Int
c2i x = x (+1) 0

c2s :: Church String -> String
c2s x = x ('*':) ""

churchAdd :: Church a  -> Church a  -> Church a
churchAdd x y = \s z -> x s (y s z)       

churchMul :: Church a -> Church a -> Church a
churchMul x y = x . y