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
                     } deriving (Show, Generic, Eq)

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

combined :: OrdList Integer
combined = evens <> odds

------------------------------  Exercise 6  ------------------------------------
type Searcher m = T.Text -> [Market] -> m

mkts :: IO [Market]
mkts = loadData

search :: Monoid m => (Market -> m) -> Searcher m
search mkToMonoid str (x:rest)
  | str `T.isInfixOf` name = mkToMonoid x <> search mkToMonoid str rest
  | otherwise = search mkToMonoid str rest
    where
      name = marketname x
search _ _ [] = mempty

testsearch = search (:[]) ("Village" :: T.Text)

------------------------------  Exercise 7  ------------------------------------
-- this just looks like good old function compostion
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.) --pointless style!!


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

firstFound :: Searcher (Maybe Market)
firstFound str allmkts = safeHead $ search (:[]) str allmkts

testfirstsearch = firstFound "Village"

------------------------------  Exercise 8  ------------------------------------
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = safeHead . reverse $ xs

lastFound :: Searcher (Maybe Market)
-- I should create a function to make these functions... ran into type errors
lastFound str allmkts = safeLast $ search (:[]) str allmkts

testlastsearch = lastFound "Village"

------------------------------  Exercise 9  ------------------------------------
allFound :: Searcher [Market]
allFound str allmkts = search (:[]) str allmkts --didnt we make this one already?

testallsearch = allFound "Village"

------------------------------  Exercise 10  ------------------------------------
numberFound :: Searcher Int
numberFound str allmkts = length $ search (:[]) str allmkts 

testNumFound = numberFound "Waikoloa"

------------------------------  Exercise 11  ------------------------------------
instance Ord Market where
  compare a b = compare (y a) (y b) -- y selects the y coord from market, due to
  -- record syntax

orderedNtoS :: Searcher [Market]
--orderedNtoS = getOrdList $ search (\x -> (OrdList [x]))
orderedNtoS = getOrdList `compose2` search (\x -> (OrdList [x]))

testordered = orderedNtoS "Village"
