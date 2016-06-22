module HW07 where

import System.Random

------------------------------  Exercise 1  ------------------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Integer]
fibs = map fib [0..]

------------------------------  Exercise 2  ------------------------------------
fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)

------------------------------  Exercise 3  ------------------------------------
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

------------------------------  Exercise 4  ------------------------------------
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList 

------------------------------  Exercise 5  ------------------------------------

-----------------  a  ------------------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-----------------  b  ------------------
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

-----------------  c  ------------------
-- this sounds a lot like iterate
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

------------------------------  Exercise 6  ------------------------------------

-----------------  a  ------------------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-----------------  b  ------------------
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

------------------------------  Exercise 7  ------------------------------------
randomList :: (Random a, RandomGen g) => g -> [a]
randomList = randoms

------------------------------  Exercise 8  ------------------------------------
randomInts :: Int -> [Int]
randomInts n = take n $ randoms (mkStdGen 1729)

------------------------------  Exercise 9  ------------------------------------
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

tonsOfInts :: [Int]
tonsOfInts = randomInts 1000000

main :: IO ()
main = print . minMax $ tonsOfInts
