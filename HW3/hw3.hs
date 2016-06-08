{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List ( sortBy )
import qualified Data.List.Split as S

---------- Exercise 1 ----------------------------------
readAsInt :: String -> Int
readAsInt x = read x :: Int

parseMessage :: String -> MaybeLogMessage
parseMessage str = par (words str)
  where par (x:xs)
          | x == "I" = ValidLM (LogMessage Info (readAsInt (head xs))
                                (unwords $ tail xs))
          | x == "W" = ValidLM (LogMessage Warning (readAsInt $ head xs)
                                (unwords $ tail xs))
          | x == "E" = ValidLM (LogMessage (Error (readAsInt (head xs)))
                                (readAsInt $ last $ take 2 xs)
                                (unwords $ drop 2 xs))
          | otherwise = InvalidLM (unwords xs)
        par [] = InvalidLM "empty input"

------------------Exercise 2----------------------------------------------------
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (x:xs) = filterValid x
  where
    filterValid (ValidLM y) = y : validMessagesOnly xs
    filterValid _ = validMessagesOnly xs

------------------Exercise 3----------------------------------------------------
parse :: String -> [LogMessage]
parse x = validMessagesOnly . map parseMessage $ lines x

------------------Exercise 4----------------------------------------------------
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ a _) (LogMessage _ b _)
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

------------------Exercise 5----------------------------------------------------
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages [] = []
sortMessages x = sortBy compareMsgs x

------------------Exercise 6----------------------------------------------------
--whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getInfo . last $ S.splitWhen helper $ sortMessages x
  where helper (LogMessage _ a _) = a < 50 
        getInfo (LogMessage _ _ b) = b
