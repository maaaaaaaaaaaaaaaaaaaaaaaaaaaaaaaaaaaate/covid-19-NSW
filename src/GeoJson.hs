{-# LANGUAGE TypeApplications, DeriveGeneric #-}

module GeoJson where

import Network.Wreq
import Data.Csv
import Data.Csv.Lens
import Data.Aeson
import Control.Lens
import Data.Geospatial
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Sequence as S

import LatLonPostcode (fetchPostcodeLatLon)

nswHealthUrl :: String
nswHealthUrl = "https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/21304414-1ff1-4243-a5d2-f52778048b29/download/covid-19-cases-by-notification-date-and-postcode-local-health-district-and-local-government-area.csv"

data CsvRow = CsvRow
  {
    postcode :: String,
    notification_date :: String
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow
instance ToJSON CsvRow

geoJsonResponse = do
  response <- get nswHealthUrl
  postcodeLatLon <- fetchPostcodeLatLon
  let csvRows = response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow
      result' = GeoFeatureCollection Nothing . S.fromList . map (makeFeature postcodeLatLon) $ csvRows
    in return $ Data.Aeson.encode result'

makeFeature pcm row = case M.lookup (postcode row) pcm of
  Just (lat, lon) -> mkGeoFeature lat lon row
  Nothing -> mkGeoFeature (-70) 150 row

mkGeoFeature lat lon row = GeoFeature Nothing
  (Point . GeoPoint . stripCRSFromPosition $ LonLat lon lat) row Nothing
