-- CIS 194 Homework 3

module Log where

import Control.Applicative ( (<$>)     )
import Text.Read           ( readMaybe )

-- | The classification of a message
data MessageType = Info
                 | Warning
                 | Error Int    -- ^ The parameter is the severity of the error
  deriving (Show, Eq)
   -- this line allows equality comparison and printing in GHCi

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
  deriving (Show, Eq)

data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM String
  deriving (Show, Eq)

data MaybeInt = ValidInt Int
              | InvalidInt
  deriving (Show, Eq)

samplelog = "I 6 Completed armadillo processing\nI 1 Nothing to report\nI 4 Everything normal\nI 11 Initiating self-destruct sequence\nE 70 3 Way too many pickles\nE 65 8 Bad pickle-flange interaction detected\nW 5 Flange is due for a check-up\nI 7 Out for lunch, back in two time steps\nE 20 2 Too many pickles\nI 9 Back from lunch\nE 99 10 Flange failed!\nThis is not the right format\nagain this should be invalid\n"

-- The functions below will be useful while completing the assignment, but
-- you are not expected to understand their details at this point.

-- Convert a @String@ to an @Int@, allowing for the possibility
-- of failure.
readInt :: String -> MaybeInt
readInt s
  | Just i <- readMaybe s = ValidInt i
  | otherwise             = InvalidInt

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO ()
testParse parse n file = do
  messages <- take n . parse <$> readFile file
  mapM_ (putStrLn . show) messages

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO ()
testWhatWentWrong parse whatWentWrong file = do
  strings <- whatWentWrong . parse <$> readFile file
  mapM_ putStrLn strings
