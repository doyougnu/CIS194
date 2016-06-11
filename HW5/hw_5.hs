{-# OPTIONS_GHC -Wall #-}
module HW05 where

import Ring
import Parser
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
  add (MKMod x) (MKMod y) = MKMod $ mod 5 (x + y)
  mul (MKMod x) (MKMod y) = MKMod $ mod 5 (x * y)

--Need to figure out the parser now
