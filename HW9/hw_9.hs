module HW09 where

import BST
import Ring
import Test.QuickCheck
import Data.List
import Control.Monad
import System.Random

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

------------------------------  Exercise 4  ------------------------------------
-- not pretty but verbose, code is data would be nice here
prop_ring :: Property
prop_ring =
  (prop1 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool) .&&.
  (prop2 :: Mat2x2 -> Mat2x2 -> Bool) .&&.
  (prop3 :: Mat2x2 -> Bool) .&&.
  (prop4 :: Mat2x2 -> Bool) .&&.
  (prop5 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool) .&&.
  (prop6 :: Mat2x2 -> Bool) .&&.
  (prop7 :: Mat2x2 -> Bool) .&&.
  (prop8 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool) .&&.
  (prop9 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)

------------------------------  Exercise 5  ------------------------------------
-- Mod5 doesnt pass 1-4
mod_prop_ring :: Property
mod_prop_ring =
  (prop1 :: Mod5 -> Mod5 -> Mod5 -> Bool) .&&.
  (prop2 :: Mod5 -> Mod5 -> Bool) .&&.
  (prop3 :: Mod5 -> Bool) .&&.
  (prop4 :: Mod5 -> Bool)

-- Mod5 passes 1 and 2
mod_prop_ring1 :: Property
mod_prop_ring1 =
  (prop1 :: Mod5 -> Mod5 -> Mod5 -> Bool) .&&.
  (prop2 :: Mod5 -> Mod5 -> Bool) 

-- Mod5 fails prop 3 the additivie identity, due to add (MkMod 5) 0 == 0 not 5
-- due to a logic error in mkMod where add (MkMod 5) 0 yields (MkMod 5) which
-- is passed to the function mkMod and gets `mod` 5, hence mkMod (MkMod 5) == 0
mod_prop_ring2 =
  (prop3 :: Mod5 -> Bool) 

------------------------------  Exercise 6  ------------------------------------
isBSTBetweenFixed :: Ord a => Maybe a   -- ^ lower bound, if one exists
             -> Maybe a                 -- ^ upper bound, if one exists
             -> BST a                   -- ^ tree to test
             -> Bool
isBSTBetweenFixed _       _       Leaf = True
isBSTBetweenFixed m_lower m_upper (Node left x right)
  = isBSTBetweenFixed m_lower  (Just x) left  &&
    isBSTBetweenFixed (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower <= x
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x <= upper
      Nothing    -> True

-- | Is this a valid BST?
isBSTFixed :: Ord a => BST a -> Bool
isBSTFixed = isBSTBetweenFixed Nothing Nothing

prop_ordered' :: BST Int -> Bool
prop_ordered' x = isBSTFixed x == is_sorted (getElements x)
  where
    is_sorted []             = True
    is_sorted [_]            = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

test' :: IO ()
test' = quickCheck prop_ordered

------------------------------  Exercise 7  ------------------------------------
genBST :: System.Random.Random a => a -> a -> Gen (BST a)
genBST lowbnd uppbnd = frequency [ (1, return Leaf)
                                 , (2,
                                    do
                                       c <- choose (lowbnd, uppbnd)
                                       leftTree <- genBST lowbnd c
                                       rightTree <- genBST c uppbnd
                                       return (Node leftTree c rightTree))] 

-- had to comment out lines 50, 51 in BST.hs and add a Num instance
-- every value in a -1...
instance (Random a, Arbitrary a) => Arbitrary (BST a) where
  arbitrary = do
    uppbnd <- arbitrary
    lowbnd <- arbitrary
    genBST lowbnd uppbnd

test :: IO ()
test = do
  sample (arbitrary :: Gen (BST Integer))
