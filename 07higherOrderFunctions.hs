import Data.Char


{-
    Question 1

    Rewrite the list comprehension 

    [f x | x <- xs, p x]

    Using map and filter
-}

huttC7Q1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
huttC7Q1 f p = map f . filter p

{-
    Question 2

    Define each of the following higher-order functions on lists. 

    all :: (a -> Bool) -> [a] -> Bool

    any :: (a -> Bool) -> [a] -> Bool

    takeWhile :: (a -> Bool) -> [a] -> [a]

    dropWhile :: (a -> Bool) -> [a] -> [a]

-}

-- All three ways (i) explicit (ii) higherorder and (iii) foldr
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x : xs) = p x && all' p xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' p = and . map p

all''' :: (a -> Bool) -> [a] -> Bool
all''' p = foldr (\x xs -> p x && xs) True

-- Some three ways (i) explicit (ii) higherorder and (iii) foldr
some :: (a -> Bool) -> [a] -> Bool
some _ [] = False
some p (x : xs) = p x || some p xs

some' :: (a -> Bool) -> [a] -> Bool
some' p = or . map p

some'' :: (a -> Bool) -> [a] -> Bool
some'' p = foldr (\x xs -> p x || xs) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs) 
    | p x       = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
    | p x       = dropWhile p xs
    | otherwise = x : xs 

{-
    Question 3

    Redefine map and filter using foldr

-}
mapfr :: (a -> b) -> [a] -> [b]
mapfr f = foldr (\x xs -> f x : xs) []

filterfr :: (a -> Bool) -> [a] -> [a]
filterfr p = foldr (\x xs -> if p x then x : xs else xs) []

{-
    Question 5

    This is nifty!

-}

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

{-
    Question 6
-}

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \t -> f (fst t) (snd t)
-- uncurry' f = \(x,y) -> f x y

{-
    Binary String Transmitter
-}

type Bit = Int 

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

makeCheck9 :: [Bit] -> [Bit]
makeCheck9 bits
    | evenParity = 0 : bits
    | otherwise  = 1 : bits
    where
        evenParity = even (sum bits)

encodeChar :: Char -> [Bit]
encodeChar = makeCheck9 . make8 . int2bin . ord

encode :: String -> [Bit]
encode = concat . map (encodeChar)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bs = take n bs : chop n (drop n bs)

checkByte :: [Bit] -> Bool
checkByte (b : bs) = b == (sum bs `mod` 2)

checkSum :: [Bit] -> Bool
checkSum bs = all checkByte (chop 9 bs)

decode :: [Bit] -> String
decode bits
    | checkSum bits = map ((chr . bin2int) . drop 1) (chop 9 bits)
    | otherwise     = error "Transmission error detected by checkbit failure."

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel []       = []
faultyChannel (b : bs) = bs

transmit :: String -> String
transmit = decode . faultyChannel . encode