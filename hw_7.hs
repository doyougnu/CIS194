module HW07 where

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
