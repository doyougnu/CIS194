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
-- test that Ring is in abelian group
---- testing + associativity
prop1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop1 x y z = add z (add x y) == add x (add y z)

---- testing + commutativity
prop2 :: (Ring a, Eq a) => a -> a -> Bool
prop2 x y = add x y == add y x

---- testing additive identity 
prop3 :: (Ring a, Eq a) => a -> Bool
prop3 x = add addId x == x

---- testing additive inverse
prop4 :: (Ring a, Eq a) => a -> Bool
prop4 x = add x (addInv x) == addId

-- test that Ring is a Monoid
---- test * associativity
prop5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop5 x y z = mul z (mul x y) == mul x (mul y z)

---- testing multpilicative identity left
prop6 :: (Ring a, Eq a) => a -> Bool
prop6 x = mul x mulId == x

---- testing multpilicative identity right
prop7 :: (Ring a, Eq a) => a -> Bool
prop7 x = mul mulId x == x

-- test that multiplication is distributive with regard to addition
---- testing left distributivity
prop8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop8 x y z = mul x (add y z) == add (mul x y) (mul x z)

---- testing right distributivity
prop9 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop9 x y z = mul (add y z) x == add (mul y x) (mul z x)
