-- Studying Graham Hutton's Book
-- Chapter 1 :: Introduction. 

{- Exercise 1

double (double 2)
    = double 2 + double 3
    = double 2 + (2 + 2)
    = (2 + 2) + (2 + 2)
    = 4 + (2 + 2)
    = 4 + 4 
    = 8

This is another possible evaluation path 
for the is expression. 
-}

{- Exercise 2
sum [x] = x + sum []
        = x + 0
        = x
-}

{- Exercise 3
product :: [Int] -> Int
product [] = 1
product (x : xs) = x * product xs

product [2,3,4] = 2 * product [3,4]
                = 2 * (3 * product [4])
                = 2 * (3 * (4 * product []))
                = 2 * (3 * (4 * 1))
                = 2 * (3 * 4)
                = 2 * 12
                = 24
-}

{- Exercise 4
qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b > x]

This can be modified by placing the qsort larger on 
the left and qsort smaller on the right.
-}

{- Exercise 5
If we replace <= with < in the original definition 
of qsort, then the sorted list will remove all 
duplicates.

One will have a sorted list, with all duplicates 
removed. This is because the duplicate values are 
not caught by either of the comprehensions smaller 
or larger. 
-}

qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
                 where
                    smaller = [a | a <- xs, a < x]
                    larger  = [b | b <- xs, b > x]


