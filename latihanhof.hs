import Text.Show.Functions

import Data.Char
import Data.Maybe
import Data.List

---------------------------------------------------------------------
-- 1. Exercises from The Craft of Functional Programming

length' :: [a] -> Int
length' xs = sum (map (\x -> 1) xs)

--

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = f . iter (n-1) f

{-
-- alternatively:
iter :: Int -> (a -> a) -> (a -> a)
iter 0 f x = x
iter n f x = f (iter (n-1) f x)
-}

--

prop_IterN x y =
  x >= 0 && y >= 0 ==>
    f1 x y == f2 x y
 where
  f1 = (\n -> iter n succ) :: Int -> Int -> Int
  f2 = (+)

--

sumSquares :: Integer -> Integer
sumSquares n = foldr (+) 0 $ map (\x -> x*x) [1..n]

--

mystery xs = foldr (++) [] (map sing xs)
 where
  sing x = [x]

prop_Mystery xs = mystery xs == (xs :: [Int])

--

{-
(id . f)  is the same as f, because the result of f is fed to the
          identity function id, and thus not changed
(id :: Bool -> Bool)

(f . id)  is the same as f, because the argument of f is fed to the
          identity function id, and thus not changed
(id :: Int -> Int)

id f      is the same as f, because applying id to something does not
          change it
(id :: (Int -> Bool) -> (Int -> Bool))
-}

--

composeList :: [a -> a] -> (a -> a)
composeList = foldr (.) id

{-
-- alternatively
composeList :: [a -> a] -> (a -> a)
composeList []     = id
composeList (f:fs) = f . composeList fs
-}

--

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x



---------------------------------------------------------------------
-- 2. List Comprehensions and Higher-order functions

prop_A1  xs    = map (+1) xs                                       == [ x+1 | x <- xs :: [Int] ]
prop_A2  xs ys = concat (map (\x -> map (\y -> x+y) ys) xs)        == [ x+y | x <- xs, y <- ys :: [Int] ]
prop_A3  xs    = map (+2) (filter (>3) xs)                         == [ x+2 | x <- xs :: [Int], x > 3 ]
prop_A4  xys   = map (\(x,_) -> x+3) xys                           == [ x+3 | (x,_) <- xys :: [(Int,Int)] ]
prop_A4' xys   = map ((+3) . fst) xys                              == [ x+3 | (x,_) <- xys :: [(Int,Int)] ]
prop_A5  xys   = map ((+4) . fst) (filter (\(x,y) -> x+y < 5) xys) == [ x+4 | (x,y) <- xys :: [(Int,Int)], x+y < 5 ]
prop_A6  mxs   = map (\(Just x) -> x+5) (filter isJust mxs)        == [ x+5 | Just x <- mxs :: [Maybe Int] ]

prop_B1  xs    = [ x+3 | x <- xs ]               == map (+3) (xs :: [Int])
prop_B2  xs    = [ x | x <- xs, x > 7 ]          == filter (>7) (xs :: [Int])
prop_B3  xs ys = [ (x,y) | x <- xs, y <- ys ]    == concat (map (\x -> map (\y -> (x,y)) (ys :: [Int])) (xs :: [Int]))
prop_B4  xys   = [ x+y | (x,y) <- xys, x+y > 3 ] == filter (>3) (map (\(x,y) -> x+y) (xys :: [(Int,Int)]))

{-
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary =
    frequency
    [ (1, do return Nothing)
    , (5, do x <- arbitrary
             return (Just x))
    ]
-}



-------------------------------------------------------------------------
-- 3. Generating Lists

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = sequence [ gen | i <- [1..n] ]

pairsOfEqualLengthLists :: Gen a -> Gen ([a],[a])
pairsOfEqualLengthLists gen =
  do n <- choose (0,100)
     xs <- listOfLength (abs n) gen
     ys <- listOfLength (abs n) gen
     return (xs,ys)

prop_ZipUnzip :: [(Int,Int)] -> Bool
prop_ZipUnzip xys =
  zip xs ys == xys
 where
  (xs,ys) = unzip xys

-- simple, but bad, solution

prop_UnzipZip :: [Int] -> [Int] -> Property
prop_UnzipZip xs ys =
  length xs == length ys ==>
    unzip (zip xs ys) == (xs,ys)

-- alternative solution 1

data TwoSameLengthLists a = SameLength [a] [a]
 deriving (Show)

instance Arbitrary a => Arbitrary (TwoSameLengthLists a) where
  arbitrary =
    do (xs,ys) <- pairsOfEqualLengthLists arbitrary
       return (SameLength xs ys)

prop_UnzipZip1 :: TwoSameLengthLists Int -> Bool
prop_UnzipZip1 (SameLength xs ys) =
  unzip (zip xs ys) == (xs,ys)

-- alternative solution 2

prop_UnzipZip2 :: Property
prop_UnzipZip2 =
  forAll (pairsOfEqualLengthLists arbitrary) $ \(xs,ys) ->
    unzip (zip xs ys) == (xs :: [Int],ys :: [Int])



-------------------------------------------------------------------------
-- 4. Generating Ordered Lists

orderedList :: Gen [Integer]
orderedList =
  do x  <- arbitrary
     ds <- arbitrary
     return (make x ds)
 where
  make x []     = []
  make x (d:ds) = x : make (x+abs d) ds
