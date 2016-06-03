{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
genBoolPairs :: Eq a => [a] -> [a] -> [Bool]
genBoolPairs = zipWith (==) 

exactMatches :: Code -> Code -> Int
exactMatches x y = sum $ map helper boollist
  where boollist = genBoolPairs x y
        helper True = 1
        helper False = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = []
countColors guess = map helper colors
  where helper z = length (filter (==z) guess)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ zipWith min a b
  where a = countColors x
        b = countColors y

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y (exactMatches x y) (matches x y - exactMatches x y)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) x = move == getMove x guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------
combinations :: Int -> [a] -> [[a]] --inefficient n choose k
combinations 0 _ = [[]]
combinations _ [] = []
combinations m (x:xs) = map (x:) (combinations (m-1) xs) ++ combinations m xs

allCodes :: Int -> [Code]
allCodes x = combinations x colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
