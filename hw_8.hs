{-# LANGUAGE FlexibleContexts #-}
module HW08 where

import Text.Read
import Data.Maybe
import Data.Char ( isDigit )
import Data.List 
import Control.Monad ( join )
import Control.Monad.Random

------------------------------  Exercise 1  ------------------------------------
isGoodDigit :: Int -> Maybe Int
isGoodDigit n
  | n < 10 = Just n
  | otherwise = Nothing

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go


-- I stumbled on this for awhile, gotta remember that every line in do must be
-- monadic
go :: String -> Maybe String
go "" = Just ""
go str = do
  n <- readMaybe . takeWhile isDigit $ str
  good_digit <- isGoodDigit n
  rest <- stripPrefix (replicate good_digit 'a') (dropWhile isDigit str)
  go rest

------------------------------  Exercise 2  ------------------------------------
specialNumbers :: [Int]
specialNumbers = [x | x <- [2..99], x `mod` 5 == 0, x `mod` 7 /= 0]

------------------------------  Exercise 3  ------------------------------------
type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
                deriving Show
type StdRand = Rand StdGen
type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

------------------------------  Exercise 4  ------------------------------------
instance Monoid ArmyCounts where
  mempty = ArmyCounts 0 0
  (ArmyCounts a b) `mappend` (ArmyCounts c d) = ArmyCounts (a + c) (b + d)

determineDelta :: [DieRoll] -> [DieRoll] -> [ArmyCounts]
determineDelta attkrs dfndrs = zipWith delta
  (sort_desc attkrs) (sort_desc dfndrs)
  where
    sort_desc = reverse . sort
    delta x y = if x > y then attackersWin else defendersWin
    attackersWin = ArmyCounts 0 (-1)
    defendersWin = ArmyCounts (-1) 0

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults as ds = foldl mappend (ArmyCounts 0 0) $ determineDelta as ds
