{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Monoid
import           GHC.Generics
import           Data.List

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

------------------------------  Exercise 1  ------------------------------------
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array v) = Array (ynToBool <$> v) -- <$> is infix fmap, translates to
ynToBool (Object v) = Object (ynToBool <$> v) -- fmap ynToBool v
ynToBool x = x

------------------------------  Exercise 2  ------------------------------------

parseData :: B.ByteString -> Either String Value
parseData filedata =
  ynToBool <$> eitherDecode filedata :: Either String Value

------------------------------  Exercise 3  ------------------------------------
data Market = Market { marketname :: T.Text
                     , x :: Float
                     , y :: Float
                     , state :: T.Text
                     } deriving (Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = eitherDecode

------------------------------  Exercise 4  ------------------------------------
jsonFile :: FilePath
jsonFile = "markets.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- some clojure style middleware
validateContents (Right value) = value
validateContents (Left cause) = fail cause

loadData :: IO [Market]
loadData = do
  filedata <- getJSON
  let parseddata = parseMarkets filedata
  fmap validateContents . return $ parseddata

------------------------------  Exercise 5  ------------------------------------
data OrdList a = OrdList { getOrdList :: [a] }
                 deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList[]
  mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs ++ ys)

--this was my recursive solution, not sure why it didn't work
--mappend (OrdList xss@(x:xs)) (OrdList yss@(y:ys))
-- | x < y = OrdList (x : mappend xs yss)
-- | otherwise = OrdList (y : mappend xss ys)

evens :: OrdList Integer
evens = OrdList [2, 4, 6]

odds :: OrdList Integer
odds = OrdList [1, 3, 5]
