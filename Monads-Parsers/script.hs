module Section7 where

import Prelude hiding (return)

type Parser a = String -> [(a, String)]

item :: Parser Char
item = \inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)]

failure :: Parser a
failure = \inp -> []

return :: a -> Parser a
return v = \inp -> [(v, inp)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]


-- parse p "abcdef"    ('a', "bcdef")

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
    [(v, out)] -> parse (f v) out
    [] -> []

p :: Parser [(Char, String)]        
p = do x <- item
       item
       y <- item
       return (x, y)


    -- import Prelude hiding (return)

    -- type Parser a = String -> [(a, String)]
    
    -- item :: Parser Char
    -- item = \inp -> case inp of
    --                 []      -> []
    --                 (x:xs)  -> [(x, xs)]
    
    -- failure :: Parser a
    -- failure = \inp -> []
    
    -- return :: a -> Parser a
    -- return v = \inp -> [(v, inp)]
    
    -- (+++) :: Parser a -> Parser a -> Parser a
    -- p +++ q = \inp -> case p inp of
    --                     []      -> parse q inp
    --                     [(v, out)] -> [(v, out)]
    
    -- parse :: Parser a -> String -> [(a, String)]
    -- parse p inp = p inp
    
    -- p :: Parser (Char, Char)
    -- p  = do x <- item
    --         item
    --         y <- item
    --         return (x,y)    