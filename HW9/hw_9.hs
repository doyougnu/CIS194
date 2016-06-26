module HW09 where

import BST
import Ring
import Test.QuickCheck
import Data.List
import Control.Monad

------------------------------  Exercise 1  ------------------------------------
instance Arbitrary Mod5 where
  arbitrary = do
    choice <- choose (0, 5)
    return (MkMod choice)
    
instance Arbitrary Mat2x2 where
  arbitrary = do
    num <- arbitrary --I wonder if this'll be the same number 4 times
    return (MkMat num num num num)

------------------------------  Exercise 2  ------------------------------------
-- no idea if this'll work...
  shrink (MkMat a b c d) = [x] ++ shrink x
    where f = head . shrinkIntegral 
          x = MkMat (f a) (f b) (f c) (f d)

------------------------------  Exercise 3  ------------------------------------
