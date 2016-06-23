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
