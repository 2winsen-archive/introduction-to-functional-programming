-- Can you check to see if a string has the same amount of 'x's and 'o's?
-- xo "ooxx" => true
-- xo "xooxx" => false
-- xo "ooxXm" => true
-- xo "zpzpzpp" => true // when no 'x' and 'o' is present should return true
-- xo "zzoo" => false

import Data.Char (toLower)

xsAgg :: Char -> Int -> Int
xsAgg curr acc
    | toLower curr == 'x' = acc + 1
    | toLower curr == 'o' = acc - 1
    | otherwise = acc

xo :: String -> Bool
xo s = foldr xsAgg 0 s == 0