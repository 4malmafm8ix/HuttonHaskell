{-
    Question 1 

    [Char] = String
    (Char,Char,Char)
    [(Bool,Char)]
    ([Bool],[Char]) = ([Bool],String)
    [[a] -> [a]]

-}

{-
    Question 2

    bools :: [Bool]
    bools = [True,False,True]

    nums :: [[Int]]
    nums = [[2,3,4],[2,3,5,7]]

    add :: Int -> Int -> Int -> Int
    add x y z = x + z

    copy :: a -> (a,a)
    copy x = (x,x)

    apply :: (a -> b) -> a -> b
    apply f = f

-}

{-
    Questions 3 & 4

    second :: [a] -> a
    second xs = head (tail xs)

    swap :: (a,b) -> (b,a)
    swap (x,y) = (y,x)

    pair :: a -> b -> (a,b)
    pair x y = (x,y)

    double :: Num a => a -> a
    double x = x*2

    palindrome :: [a] -> Bool
    palindrome xs = reverse xs == xs

    twice :: (a -> a) -> a -> a
    twice f x = f (f x)

-}

