import Test.QuickCheck
import Text.Show.Functions

import Data.Char
import Data.Maybe
import Data.List

>>> length' :: [a] -> Int
>>> length' xs = sum (map (\x -> 1) xs)
<interactive>:1818:2-22: error:
    • No instance for (Show ([a0] -> Int))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
<BLANKLINE>
<interactive>:1819:26: warning: [-Wunused-matches]
    Defined but not used: ‘x’

>>> iter 0 f = id
>>> iter n f x = f (iter (n - 1) f x)
<interactive>:1827:2-14: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘iter’:
        Patterns not matched: p _ where p is not one of {0}
<BLANKLINE>
<interactive>:1827:9: warning: [-Wunused-matches]
    Defined but not used: ‘f’


>>> 5 :: Double
5.0

>>> listsum [] = 0
>>> listsum (x:xs) = x + listsum xs
>>> listsum 

>>> length ['a','b','b']
3

>>> lengthcoba :: [a] -> Int
>>> lengthcoba ['a','b','b']
<interactive>:1802:2-11: error:
    Variable not in scope: lengthcoba :: [a1] -> Int
<BLANKLINE>
<interactive>:1803:2-11: error:
    Variable not in scope: lengthcoba :: [Char] -> t

>>> map snd [(1,2),(3,4)]
[2,4]


>>> :t gcd 15 20
gcd 15 20 :: Integral a => a

--- void data type
>>> :t ()
() :: ()

>>> [2, 10 .. 99]
>>> [3, 7 .. 233]
>>> ['a','k','u']
>>> zip [1 .. 5] [True, False]
>>> putStr "Haha"
>>> map (+2)
[2,10,18,26,34,42,50,58,66,74,82,90,98]
[3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,67,71,75,79,83,87,91,95,99,103,107,111,115,119,123,127,131,135,139,143,147,151,155,159,163,167,171,175,179,183,187,191,195,199,203,207,211,215,219,223,227,231]
"aku"
[(1,True),(2,False)]
Haha

module Main where
    factorial n = if n == 0 then 1 else n * factorial (n-1)

    main = do putStrLn "What is 5!"
        x <- readLn
        if x == factorial 5
            then putStrLn "Right"
            else putStrLn "Wrong"

>>> fst (1,2)
1

>>> sum ([1,2,3])
6

>>> xs = [1,2,3,4]
>>> map (+2) (map (+1) xs)
[4,5,6,7]

>>> :t succ 33
succ 33 :: (Enum a, Num a) => a


>>> ['a','b' .. 'z'] :: [Char]
"abcdefghijklmnopqrstuvwxyz"


>>> [(i,j) | i <- [1,2], j <- [1..4]]
[(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]

>>> take 15 [(i,j) | i <- [1,2], j <- [1..]]
[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(1,11),(1,12),(1,13),(1,14),(1,15)]




