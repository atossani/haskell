module HotelAssignment where

import NamedCSVParser
import qualified Data.ByteString.Lazy as BL

main = do  
  google <- BL.readFile "./google.csv"
  wyndham <- BL.readFile "./wyndham.csv"
  let grecords = googleRecords google
  let wrecords = wyndhamNamedRecords wyndham
  print $ returnBrand grecords wrecords

returnBrand :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
returnBrand [] _ = []
returnBrand (g:gs) ws = (groupName g, brands g ws) : returnBrand gs ws
  where
    brands grec = (map brand).filter (\w -> hotelId grec == site w)

ungroup :: [GoogleRecord] -> [WyndhamRecord] -> [(GroupName, [Brand])]
ungroup gs ws = filter (\(a,b) -> b == []) $ returnBrand gs ws

missing :: [WyndhamRecord] -> [GoogleRecord] -> [Site]
missing ws gs = map snd $ filter (\(a,b) -> a == []) $ returnMissing ws gs
    where
        returnMissing [] _ = []
        returnMissing (w:ws) gs = (groupnames w gs, site w) : returnMissing ws gs
        groupnames wrec = (map groupName).filter (\w -> site wrec == hotelId w)


-- returnMissing [] _ = []
-- returnMissing (w:ws) gs = (groupnames w gs, brand w) : returnMissing ws gs
--   where
--     groupnames wrec = (map groupName).filter (\w -> site wrec == hotelId w)

-- Find records where brand and group are the same
-- filter (\(a,b) -> a == b) $ returnBrand grecords wrecords

-- Find records where brand and group are different
-- filter (\(a,b) -> a /= b) $ returnBrand grecords wrecords