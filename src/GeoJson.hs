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
nswHealthUrl = "https://data.nsw.gov.au/data/dataset/97ea2424-abaf-4f3e-a9f2-b5c883f42b6a/resource/2776dbb8-f807-4fb2-b1ed-184a6fc2c8aa/download/covid-19-cases-by-notification-date-location-and-likely-source-of-infection.csv"

data CsvRow = CsvRow
  {
    postcode :: String,
    notification_date :: String,
    likely_source_of_infection :: String
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow
instance ToJSON CsvRow

testGetLoc = do
  response <- get nswHealthUrl
  postcodeLatLon <- fetchPostcodeLatLon
  return $ response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow

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
