{-# LANGUAGE TypeApplications, DeriveGeneric #-}

module InfectionSource where

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
nswHealthUrl = "https://data.nsw.gov.au/data/dataset/c647a815-5eb7-4df6-8c88-f9c537a4f21e/resource/2f1ba0f3-8c21-4a86-acaf-444be4401a6d/download/covid-19-cases-by-notification-date-and-likely-source-of-infection.csv"

data CsvRow = CsvRow
  {
    notification_date :: String,
    likely_source_of_infection :: String
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow
instance ToJSON CsvRow

fetchInfectionSource = do
  response <- get nswHealthUrl
  let csvRows = response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow
    in return $ csvRows
