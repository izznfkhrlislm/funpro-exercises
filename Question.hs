import Data.Char
import Data.Maybe
import Data.List

factorial n = if n == 0 then 1 else n * factorial (n-1)

listsum [] = 0
listsum (x:xs) = x + listsum(xs)

-- How does it work: changing all ints in list parameter to 1 and count the sum (which equals object count)
lengthxs :: [a] -> Int
lengthxs xs = sum(map(\x -> 1) xs)

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

sumSquares :: Integer -> Integer
sumSquares n = foldr (+) 0 $ map (\x -> x*x) [1..n]

mystery xs = foldr (++) [] (map sing xs)
    where
        sing x = [x]

composeList :: [a -> a] -> (a -> a)
composeList [] = id
composeList(f:fs) = f . composeList fs

flip1 :: (a -> b -> c) -> (b -> a -> c)
flip1 f = \x y -> f y x

listcomp1r xs = [ x + 1 | x <- xs :: [Int] ]
listcomp1 xs = map (+1) xs

{-
    listcomp2 --> counting sum of x and y in every array inputs
    [x1,y1,z1] [x2,y2.z2] = [x1+x2,x1+y2,x1+z2,y1+x2,y1+y2,y1+z2,z1+x2,z1+y2,z1+z2]
-}
listcomp2r xs ys = [ x + y | x <- xs, y <- ys :: [Int]]
listcomp2 xs ys = concat (map (\x -> map (\y -> x + y) ys) xs)

{-
    listcomp3 --> adding every element in list xs with 2, and filtering sum more than 3
-}
listcomp3r xs = [ x + 2 | x <- xs :: [Int], x > 3]
listcomp3 xs = map (+2) (filter (> 3) xs)

{-
    listcomp4 --> adding every first element in tuple list with 3, and store it in a list
    [(x1,y1),(x2,y2)] = [x1+3,x2+3]
-}
listcomp4r xys = [ x + 3 | (x,_) <- xys :: [(Int,Int)]]
listcomp4 xys = map (\(x,_) -> x + 3) xys

{-
    listcomp5 --> checking sum of pair (x,y) in input if it less than 5, and adding first element x with 4
-}
listcomp5r xys = [ x + 4 | (x,y) <- xys :: [(Int,Int)], x+y < 5]
listcomp5 xys = map ((+4) . fst) (filter (\(x,y) -> x + y < 5) xys)

{-
    listcomp6 --> Receiving list of Just Int data type and returning a list of Int in input list with 5
    [Just x1, Just x2, Just x3] --> [x1+5,x2+5,x3+5]
-}
listcomp6r mxs = [ x + 5 | Just x <- mxs ]
listcomp6 mxs = map (\(Just x) -> x + 5) mxs

hof1r xs = map (+3) xs
hof1 xs = [ x + 3 | x <- xs :: [Int]]

hof2r xs = filter (>7) xs
hof2 xs = [x | x <- xs, x > 7]

hof3r xs ys = (map (\x -> map (\y -> (x,y)) ys) xs)
hof3 xs ys = [(x,y) | x <- xs, y <- ys :: [Int]]

{-
    hof4 --> Sum pair (x,y) in a list input, and return list of sum pair more than 3
-}
hof4r xys = filter (>3) (map (\(x,y) -> x+y) xys)
hof4 xys = [ x + y | (x,y) <- xys :: [(Int, Int)], x+y > 3]


