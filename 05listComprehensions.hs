import Data.Char
{-
    Caesar Cipher
-}

count :: Eq a => a -> [a] -> Int
count a as = length [1 | x <- as, a == x]

lowers :: String -> Int 
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (ord 'a' + i)

shift :: Int -> Char -> Char
shift i c 
    | isLower c = int2let ((let2int c + i) `mod` 26)
    | otherwise = c

caesarEncode :: Int -> String -> String
caesarEncode i xs = [shift i x | x <- xs]

caesarDecode :: Int -> String -> String 
caesarDecode i = caesarEncode (-i)

-- Expected letter frequencies for English text. 
englishFreq :: [Float]
englishFreq = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
              0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
              6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqTable :: String -> [Float]
freqTable s = [percent (count x s) n | x <- ['a'..'z']]
                where
                    n = lowers s

chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [((x - y)^2)/y | (x,y) <- zip xs ys]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crackCaesar :: String -> String
crackCaesar xs = caesarDecode key xs
    where
        key = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n obsfreq) englishFreq | n <- [0..25]]
        obsfreq = freqTable xs

{-
    Question 1
-}

sumSquares :: Int
sumSquares = sum [x^2 | x <- [1..100]]

{-
    Question 2
-}

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m],y <- [0..n]]

{-
    Question 3
-}

square :: Int -> [(Int,Int)]
square m = [(x,y) | (x,y) <- grid m m, x /= y]

{-
    Question 4
-}

replicate :: Int -> a -> [a]
replicate n c = [c | _ <- [1..n]]

{-
    Question 5
-}

pythagoreanTriples :: Int -> [(Int,Int,Int)]
pythagoreanTriples bound = [(x,y,z) | x <- [1..bound], y <- [1..bound], z <- [1..bound],
                                        x^2 + y^2 == z^2]

{-
    Question 6
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool 
perfect n = sum (tail $ reverse $ factors n) == n

perfects :: Int -> [Int]
perfects bound = [x | x <- [1..bound], perfect x]

{-
    Question 7

    [(x,y) | x <- [1,2], y <- [3,4]]

    Rewrite this with nested comprehensions each on 
    a single generator. 

    ** First was wrong order **
    concat [[(x,y) | x <- [1,2]] | y <- [3,4]]  
-}

huttC5Q7 :: [(Int,Int)]
huttC5Q7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

{-
    Question 8
-}

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 v vs = find v (zip vs [0..])

{-
    Question 9
-}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]
