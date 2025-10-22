{-
    Question 1
-}  
    
halve :: [a] -> ([a],[a])
halve xs = splitAt ((length xs) `div` 2) xs

{-
    Question 2

-}

thirda :: [a] -> a
thirda = head . tail . tail

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc (_:_:c:cs) = c

{-
    Question 3

-}

safetailA :: [a] -> [a]
safetailA xs = if null xs 
                then [] 
                else tail xs

safetailB :: [a] -> [a]
safetailB xs 
    | null xs   = []
    | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs  = tail xs

{-
    Question 4

-}

orA :: Bool -> Bool -> Bool
orA True  _ = True
orA False x = x

orB :: Bool -> Bool -> Bool
orB True True    = True 
orB True False   = True
orB False True   = True 
orB False False  = False

orC :: Bool -> Bool -> Bool
orC False False = False 
orC _ _         = True

orD :: Bool -> Bool -> Bool 
orD x y
    | x == y    = x
    | otherwise = True

{-
    Question 5

-}

conditionalConj1 :: Bool -> Bool -> Bool 
conditionalConj1 a b = if a == True 
                        then if b == True
                                then True
                                else False
                        else False

{-
    Question 6

-}

conditionalConj2 :: Bool -> Bool -> Bool
conditionalConj2 a b = if a == True then b else a

{-
    Question 7

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z
-}

mult :: Int -> Int -> Int -> Int 
mult = \x -> (\y -> (\z -> x*y*z))

{-
    Question 8

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if check then True else False
                where
                    checkSum = luhnDouble a + b + luhnDouble c + d
                    check    = checkSum `mod` 10 == 0

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
    | check     = True
    | otherwise = False
        where
            checkSum = luhnDouble a + b + luhnDouble c + d
            check    = checkSum `mod` 10 == 0

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = check
        where
            checkSum = luhnDouble a + b + luhnDouble c + d
            check    = checkSum `mod` 10 == 0

-}

luhnDouble :: Int -> Int
luhnDouble x = if dub > 9 then dub - 9 else dub
                where
                    dub = x + x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = checkSum `mod` 10 == 0
        where
            checkSum = luhnDouble a + b + luhnDouble c + d
