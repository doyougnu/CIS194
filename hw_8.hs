module HW08 where

import Text.Read
import Data.Maybe
import Data.Char ( isDigit )
import Data.List
import Control.Monad 
import Control.Monad.Random
import Control.Applicative

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

------------------------------  Exercise 5  ------------------------------------
dieRolls :: Army -> [StdRand DieRoll]
dieRolls 0 = []
dieRolls x = replicate x dieRoll

updateArmyCount :: Int -> Int -> ArmyCounts -> ArmyCounts
updateArmyCount att def as = as `mappend` (ArmyCounts att def)

negCheck :: ArmyCounts -> ArmyCounts
negCheck army = updateArmyCount new_as new_ds army
  where
    new_as
      | (attackers army) < 0 = (-1) * (attackers army)
      | otherwise = 0 --dont change current value
    new_ds
      | (defenders army) < 0 = (-1) * (defenders army)
      | otherwise = 0



-- pretty straightforward but not that pretty
battle :: ArmyCounts -> StdRand ArmyCounts
battle x =
  do
  let attkrs
        | (attackers x) < 3 = attackers x
        | (attackers x) == 3 = 2
        | otherwise = 3

  let dfndrs = if (defenders x > 2) then 2 else (defenders x)
  att_rolls <- sequence . dieRolls $ attkrs
  dfn_rolls <- sequence . dieRolls $ dfndrs
  battle_results <- return (battleResults att_rolls dfn_rolls)
  return (negCheck $ mappend x battle_results)

------------------------------  Exercise 6  ------------------------------------
-- Monads are pretty cool
invade :: ArmyCounts -> StdRand ArmyCounts
invade x
  | (attackers x < 2) || (defenders x <= 0) = return x
  | otherwise = battle x >>= invade

------------------------------  Exercise 7  ------------------------------------
(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

attackersWin :: ArmyCounts -> Int
attackersWin x
   | defenders x == 0 = 1
   | otherwise = 0

successProb :: ArmyCounts -> StdRand Double
successProb army = do
  battles <- sequence . replicate 1000 . invade $ army
  battles_won <- return . sum $ map attackersWin battles
  return (battles_won // 1000)

