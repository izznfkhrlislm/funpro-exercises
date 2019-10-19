import Data.List

divisor n = [x | x <- [1..n], n `mod` x == 0]

quickSort [] = []
quickSort (x:xs) = quickSort([y | y <- xs, y <= x]) ++ [x] ++ quickSort([y | y <- xs, y > x])

perm [] = [[]]
perm ls = [x:ps | x <- ls, ps <- perm(ls\\[x])]

pythagoras = [(x,y,z) | z <- [5 ..], y <- [4 .. z-1], x <- [3 .. z-2], x*x + y*y == z*z]

>>> take 10 pythagoras
[(3,4,5),(8,6,10),(6,8,10),(5,12,13),(12,9,15),(9,12,15),(15,8,17),(8,15,17),(16,12,20),(12,16,20)]


