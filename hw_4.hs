{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.Char

data BST a = Leaf | Node (BST a) a (BST a) deriving (Show)

--skipping exercises 1-12
---------------------------- Exercise 13 ---------------------------------------
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ a Leaf = Node (Leaf) a (Leaf)
insertBST f x (Node l nodeval r)
  | x `f` nodeval == EQ = Node l x r
  | x `f` nodeval == LT = Node (insertBST f x l) nodeval r
  | x `f` nodeval == GT = Node l nodeval (insertBST f x r)

testBST :: (Ord a) => a -> a -> Ordering
testBST x y
  | x < y = LT
  | x > y = GT
  | x == y = EQ

---------------------------- Exercise 14 ---------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

allCaps :: [String] -> Bool
allCaps [] = True
allCaps xs = all hasLeadingCaps xs
  where
    hasLeadingCaps [] = False
    hasLeadingCaps (y:_) = isUpper y

---------------------------- Exercise 15 ---------------------------------------
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace [] = []
dropTrailingWhitespace str = filter isAlpha str

---------------------------- Exercise 16 ---------------------------------------
firstChar :: String -> Char
firstChar [] = ' '
firstChar (x:_) = x

firstLetters :: [String] -> [Char]
firstLetters [] = []
firstLetters xs = map firstChar xs

---------------------------- Exercise 17 ---------------------------------------
bookend :: String -> String
bookend x = "[" ++ x ++ "]"

endWithaComma :: [String] -> [String]
endWithaComma [] = []
endWithaComma (x:[]) = [x]
endWithaComma (x:xs) = [x ++ ","] ++ endWithaComma xs

asList :: [String] -> String
asList [] = ""
asList xs = bookend $ unwords $ endWithaComma xs
