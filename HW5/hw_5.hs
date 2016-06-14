{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW05 where

import Ring
import Parser
import Data.Char

---------------------------------- Exercise 1 ----------------------------------
--Don't know what this is asking me to dointParsingWorks :: Bool

intParsingWorks :: Bool
intParsingWorks =
  (parse "3" == Just (3 :: Integer, "")) &&
  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
  (addId == (0 :: Integer))

---------------------------------- Exercise 2 ----------------------------------
data Mod5 = MKMod Integer deriving (Show, Eq)

instance Num Mod5 where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs = abs
  signum = signum
  fromInteger = MKMod

instance Ring Mod5 where
  addId = 0
  addInv = negate
  mulId = 1
  add (MKMod x) (MKMod y) = MKMod $ mod (x + y) 5
  mul (MKMod x) (MKMod y) = MKMod $ mod (x * y) 5

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

instance Parsable Mod5 where
  parse str
    | Just s <- safeHead str = Just (MKMod (toInteger $ digitToInt s)
                                    , drop 1 str)
    | otherwise = Nothing

ringParsingWorks :: Bool
ringParsingWorks =
  -- test that simple parsing works
  (parse "4" == Just (MKMod 4, "")) &&
  -- test that an expression works
  (parseRing "3 + 4" == Just (MKMod 2)) &&
  -- now test the Ring rules hold
  (add (MKMod 1) addId == (MKMod 1)) &&
  -- testing add inverse property now
  (add (MKMod 5) (addInv (MKMod 5)) == addId) &&
  -- testing multiplicative identity property
  (mul (MKMod 3) mulId == MKMod 3) &&
  -- testing associative property and commutative property
  (add (add (MKMod 2) (MKMod 3)) (MKMod 1) == (add (MKMod 2) (add (MKMod 1)
                                                              (MKMod 3)))) &&
  (mul (mul (MKMod 2) (MKMod 3)) (MKMod 1) == (mul (MKMod 2) (mul (MKMod 1)
                                                              (MKMod 3)))) &&
  -- testing distributive property
  (mul (MKMod 2) (add (MKMod 3) (MKMod 5)) == (add (mul (MKMod 2) (MKMod 3))
                                              (mul (MKMod 2) (MKMod 5)))) &&
  (mul (add (MKMod 2) (MKMod 4)) (MKMod 1) == (add (mul (MKMod 2) (MKMod 1))
                                              (mul (MKMod 4) (MKMod 1))))

---------------------------------- Exercise 3 ----------------------------------
data Mat2x2 = Mat2x2 MatRow MatRow deriving (Show, Eq)
type MatRow = (Int, Int)

instance Ring Mat2x2 where
  addId = Mat2x2 (0, 0) (0, 0)
  addInv (Mat2x2 (a, b) (c, d)) = Mat2x2 (negate a, negate b) (negate c, negate d)
  mulId = Mat2x2 (1, 1) (1, 1)
  add (Mat2x2 (a, b) (c, d)) (Mat2x2 (e, f) (g, h)) = Mat2x2 (p, q) (r, s)
    where
      p = a + e
      q = b + f
      r = c + g
      s = d + h
  mul (Mat2x2 (a, b) (c, d)) (Mat2x2 (e, f) (g, h)) = Mat2x2 (p, q) (r, s)
    where
      p = a * e + b * g
      q = a * f + b * h
      r = c * e + d * g
      s = c + f + d * h
 
-- this is so ugly there has to be a more idiomatic solution
matParseHelper :: [Char] -> Maybe (Mat2x2, [Char])
matParseHelper (a:b:c:d:rest) = Just ((Mat2x2 (digitToInt a, digitToInt b)
  (digitToInt c , digitToInt d)), rest)
matParseHelper _ = Nothing

instance Parsable Mat2x2 where
  parse = matParseHelper . filter isNumber

-- Now testing
matRingParsingWorks :: Bool
matRingParsingWorks = (parse "[[1,2][3,4]]" == Just (Mat2x2 (1,2) (3,4), ""))
