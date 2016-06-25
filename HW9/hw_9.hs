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
    num <- arbitrary
    return (MkMat num num num num)
