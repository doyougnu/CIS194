{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW05 where

import Ring
import Parser
import Data.Char
import Data.Maybe ( listToMaybe )

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
data Mat2x2 = Mat2x2 MatRow MatRow | Scalar Int deriving (Show, Eq)
type MatRow = (Int, Int)

instance Ring Mat2x2 where
  addId = Mat2x2 (0, 0) (0, 0)

  addInv (Mat2x2 (a, b) (c, d)) = Mat2x2 (negate a, negate b) (negate c, negate d)
  addInv (Scalar z) = Scalar (negate z)

  mulId = Mat2x2 (1, 0) (0, 1)

  add (Mat2x2 (a, b) (c, d)) (Mat2x2 (e, f) (g, h)) = Mat2x2 (p, q) (r, s)
    where
      p = a + e
      q = b + f
      r = c + g
      s = d + h
  add (Scalar n) (Mat2x2 (a, b) (c, d)) = Mat2x2 (n * a, n * b) (n * c, n * d)
  add (Mat2x2 (a, b) (c, d)) (Scalar n) = Mat2x2 (n * a, n * b) (n * c, n * d)
  add (Scalar n) (Scalar m)  = Scalar (n * m)

  mul (Mat2x2 (a, b) (c, d)) (Mat2x2 (e, f) (g, h)) = Mat2x2 (p, q) (r, s)
    where
      p = a * e + b * g
      q = a * f + b * h
      r = c * e + d * g
      s = c * f + d * h
  mul (Scalar s) (Mat2x2 (a, b) (c, d)) = Mat2x2 (s * a, s * b) (s * c, s * d)
  mul (Mat2x2 (a, b) (c, d)) (Scalar s)  = Mat2x2 (s * a, s * b) (s * c, s * d)
  mul (Scalar s) (Scalar n)  = Scalar (s * n)
 
-- this is so ugly there has to be a more idiomatic solution
toMat2x2 :: [Char] -> Mat2x2
toMat2x2 (a:b:c:d:_) = Mat2x2 (digitToInt a, digitToInt b)
  (digitToInt c , digitToInt d)
toMat2x2 _ = addId --this is a poor decision, should be a maybe monad

instance Parsable Mat2x2 where
  parse str 
    | any isSpace str == True = Just (toMat2x2 . filter isNumber . head $
                                      parsed_str, unwords . tail $ parsed_str)
    | isNumber . head $ str = Just (Scalar (digitToInt . head $ str), "")
    | any isSpace str == False = Just (toMat2x2 $ filter isNumber str, "")  
    | otherwise = Nothing
    where parsed_str = words str 

-- Now testing
matRingParsingWorks :: Bool
matRingParsingWorks =
  --test parsing
  (parse "[[1,2][3,4]]" == Just (Mat2x2 (1,2) (3,4), "")) &&
  (parseRing "[[1,2][3,2]] * [[0,0][0,0]]" == Just (Mat2x2 (0,0) (0,0))) &&
  (parseRing "[[1,2][3,2]] + [[1,1][1,1]]" == Just (Mat2x2 (2,3) (4,3))) &&
  -- test Ring rules, starting with + associative
  (add (add a b) c) == (add a (add b c)) &&
  -- testing additive identity
  ((add a addId) == a) &&
  -- testing additive inverse
  (add a (addInv a) == addId) &&
  -- testing  + commutative
  (add a b == add b a) &&
  -- testing * associative
  (mul (mul a b) c == mul a (mul b c)) &&
  -- testing multicative identity
  (mul a mulId == a) &&
  -- testing * distributive property over +
  (mul a (add b c) == add (mul a b) (mul a c)) &&
  (mul (add b c) a == add (mul b a) (mul c a))
  where
    a = Mat2x2 (1, 1) (1, 1)
    b = mul (Scalar 2) a
    c = mul (Scalar 3) a

---------------------------------- Exercise 4 ----------------------------------

instance Ring Bool where
  addId = True
  addInv = not
  mulId = True
  add = xor
  mul = (&&)

instance Parsable Bool where
  parse = listToMaybe . reads

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

-- now just need to test
