module HotelAssignment where

import CSVParser
import qualified Data.ByteString.Lazy as BL

main = do  
  google <- BL.readFile "./google.csv"
  wyndham <- BL.readFile "./wyndham.csv"
  let grecords = googleRecords google
  let wrecords = wyndhamRecords wyndham
  print $ returnBrand grecords wrecords

returnBrand :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
returnBrand [] _ = []
returnBrand (g:gs) ws = (groupName g, brands g ws) : returnBrand gs ws
  where
    brands grec = (map brand).filter (\w -> hotelId grec == site w)

ungroup :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
ungroup gs ws = filter (\(a,b) -> b == []) $ returnBrand gs ws

missing :: [WyndhamRecord] -> [GoogleRecord] -> [([GroupName], Brand)]
missing ws gs = filter (\(a,b) -> b == []) $ returnMissing ws gs
    where
        returnMissing (w:ws) gs = (groupnames w gs, brand w) : returnMissing ws gs
        groupnames wrec = (map groupName).filter (\w -> site wrec == hotelId w)

-- Find records where brand and group are the same
-- filter (\(a,b) -> a == b) $ returnBrand grecords wrecords

-- Find records where brand and group are different
-- filter (\(a,b) -> a /= b) $ returnBrand grecords wrecords