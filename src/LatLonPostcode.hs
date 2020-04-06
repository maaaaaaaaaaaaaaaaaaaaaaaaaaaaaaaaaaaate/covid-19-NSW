{-# LANGUAGE TypeApplications, DeriveGeneric #-}

module LatLonPostcode where

import Data.Csv
import Control.Lens
import Data.Csv.Lens
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.Map.Lens

-- todo change to (lon,lat)

data CsvRow = CsvRow
  {
    postcode :: String,
    lat :: Double,
    lon :: Double
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow

fetchPostcodeLatLon :: IO (M.Map String (Double, Double))
fetchPostcodeLatLon = do
  csv' <- BS.readFile "Australian_Post_Codes_Lat_Lon.csv"
  let rows' = csv' ^.. namedCsv . rows . _NamedRecord @CsvRow
      -- calculate average of postcode (lat,lon) sums
      result' = M.map average' . foldl aggregate' M.empty $ rows'

      aggregate' :: M.Map String (Int, [(Double, Double)]) -> CsvRow -> M.Map String (Int, [(Double, Double)])
      aggregate' acc v = case M.lookup (postcode v) acc of
        Just (c, xs) -> M.insert (postcode v) (c+1, (lat v, lon v):xs) acc
        Nothing -> M.insert (postcode v) (1, [(lat v, lon v)]) acc

      average' :: (Int, [(Double, Double)]) -> (Double, Double)
      average' (d, ls) = (sum (map fst ls)/(fromIntegral d), sum (map snd ls)/(fromIntegral d))

  return result'
