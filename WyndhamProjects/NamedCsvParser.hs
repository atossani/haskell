{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CSVParser where

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import GHC.Generics (Generic)
import Data.List
import Data.Csv


type EitherGoogle  = Either String (Vector GoogleRecord)
type EitherWyndham = Either String (Vector WyndhamRecord)

type Brand = String
type GroupName = String
type Site = Integer


data GoogleRecord = BadGoogleRecord |
                    GRec {accountId :: !Integer,
                          accountName :: !String,
                          groupName :: !String,
                          hotelId :: !Integer,
                          hotelName :: !String} deriving (Generic, Show)

data WyndhamRecord = BadWyndhamRecord | 
                     WRec {brand :: !String, site :: !Integer} deriving (Generic, Show)


instance FromRecord GoogleRecord

instance FromNamedRecord WyndhamRecord where
    parseNamedRecord m = WRec <$> m .: "Brand." <*> m .: "Site."

googleRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode NoHeader csv :: EitherGoogle

wyndhamNamedRecords = toList.(fromRight empty).parseCsv
  where parseCsv csv = decode HasHeader csv :: EitherWyndham

--- Testing
main = do 
  wyndham <- BL.readFile "./wyndham.csv"
  let wrecords = wyndhamNamedRecords wyndham
  print $ map site wrecords