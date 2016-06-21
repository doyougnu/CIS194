{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Monoid
import           GHC.Generics
import           Data.List ( isInfixOf )

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

------------------------------  Exercise 1  ------------------------------------
-- read JSON as ByteString
-- parse ByteString using eitherDecode

jsonFile :: FilePath
jsonFile = "markets.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

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


