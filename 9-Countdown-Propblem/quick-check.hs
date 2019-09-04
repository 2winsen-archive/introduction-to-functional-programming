import Test.QuickCheck

len :: [a] -> Int
len [] = 0
len xs | length xs == 10 = 9
       | otherwise = length xs

test_len = quickCheck ((\s -> len s == length s) :: [Char] -> Bool)


splitAt2 :: Int -> [a] -> ([a], [a])
splitAt2 n [] = ([], [])
splitAt2 n xs = (take n xs, drop n xs)

prop_splitAt2 :: Int -> [Int] -> Bool
prop_splitAt2 n xs = fstSplitted ++ snd splitted == xs && length fstSplitted == n
    where splitted = splitAt2 n xs
          fstSplitted = fst splitted
test_splitAt2 = quickCheck prop_splitAt2