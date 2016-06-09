{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List ( sortBy
                 , isInfixOf )
import Data.Char

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
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong x = map getInfo . filter helper $ sortMessages x
  where helper (LogMessage (Error a) _ _) = a >= 50
        helper LogMessage{} = False
        getInfo (LogMessage _ _ b) = b

------------------Exercise 7----------------------------------------------------
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _ [] = []
messagesAbout str logs = filter onlyMatches logs
  where onlyMatches (LogMessage Info _ _) = False
        onlyMatches (LogMessage (Error _) _ s) = isInfixOf lstr $ map toLower s
        onlyMatches (LogMessage Warning _ s) = isInfixOf lstr $ map toLower s
        lstr = map toLower str
