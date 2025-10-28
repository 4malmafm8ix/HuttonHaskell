{-
    Question 1

    fac :: Int -> Int
    fac 0 = 1
    fac n = n * fac (n-1)

    fac (-1) = -1 * fac (-2)
             = -1 * (-2 * fac (-3))
             = ...

    
    fac :: Int -> Int
    fac 0 = 1
    fac n
        | n > 0     = n * fac (n-1)
        | otherwise = 0

-}

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

{-
    Question 2
-}

sumDown :: Int -> Int 
sumDown 0 = 0
sumDown n = n + sumDown (n-1)

{-
    Question 3

    (^) :: Int -> Int -> Int
    m ^ 0 = 1
    m ^ n = m * (m ^ (n-1))

    2 ^ 3 = 2 * (2 ^ 2)
          = 2 * (2 * (2 ^ 1))
          = 2 * (2 * (2 * (2 ^ 0)))
          = 2 * (2 * (2 * 1))
          = 8

-}

{-
    Question 4

    euclid :: Int -> Int -> Int
    euclid n 0 = n
    euclid 0 m = m
    euclid n m
        | n == m    = n
        | n < m     = euclid n (m - n)
        | otherwise = euclid m (n - m)


    Answer in the book: 
        euclid :: Int -> Int -> Int
        euclid x y | x == y = x
                | x < y = euclid x (y - x)
                | x > y = euclid y (x - y)
    This fails to terminate when one of the inputs 
    is zero. For example: 

    euclid 3 0 

    However this should return 3. 
    This means the bases cases of the recursion are not 
    given appropriately!

-}

euclid :: Int -> Int -> Int
euclid n 0 = n
euclid 0 m = m
euclid n m | n == m    = n
           | n < m     = euclid n (m - n)
           | otherwise = euclid m (n - m)

{-
    Question 5

    length [1,2,3]
        = 1 + length [2,3]
        = 1 + (1 + length [3])
        = 1 + (1 + (1 + length []))
        = 1 + (1 + (1 + 0))
        = 3

    drop 3 [1,2,3,4,5]
        = drop 2 [2,3,4,5]
        = drop 1 [3,4,5]
        = drop 0 [4,5]
        = [4,5]

    init [1,2,3]
        = 1 : init [2,3]
        = 1 : 2 : init [3]
        = 1 : 2 : []
        = [1,2]

-}

{-
    Question 6
-}

andd :: [Bool] -> Bool
andd [] = True
andd (x : xs) = x && andd xs

concatt :: [[a]] -> [a]
concatt []     = []
concatt (x : xs) = x ++ concatt xs

replicatee :: Int -> a -> [a]
replicatee 0 _ = []
replicatee n x  = x : replicatee (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (x : xs) 0 = x
(!!!) (x : xs) n = xs !!! (n - 1)

elemm :: Eq a => a -> [a] -> Bool
elemm t []                   = False
elemm t (x : xs) | t == x    = True
                 | otherwise = elemm t xs


{-
    Question 7
-}

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | y < x  = y : merge (x : xs) ys

{-
    Question 8
-}

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (first,second)
    where
        half   = (length xs + 1) `div` 2
        first  = take half xs
        second = drop half xs

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort first) (mergeSort second)
    where
        (first,second) = halve xs