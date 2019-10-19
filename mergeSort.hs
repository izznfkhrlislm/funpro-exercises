{-
    implementing merge sort algorithm on Haskell
    merge: merging two sublist that has been sorted
    halve: splitting input list into half
-}

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve xs = (take lhx xs, drop lhx xs)
            where lhx = length xs `div` 2

msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
            where (left, right) = halve xs