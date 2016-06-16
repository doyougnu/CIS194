{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import           Data.Aeson
import           Data.Monoid
import           GHC.Generics

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

what :: (T.Text, String, T.Text) -> (T.Text, String, Bool)
what (x, String "Y") = (x, String True)

--ynToBool :: Value -> Value

main = do
  -- <$> is an applicative functor, just maps eitherDecode of the JSON
  result <- (eitherDecode <$> getJSON) :: IO (Either String [Value])
  let x = take 1 <$> result
  return x

