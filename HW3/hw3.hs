{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
