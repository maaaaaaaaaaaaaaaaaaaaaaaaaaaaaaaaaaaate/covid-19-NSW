{-# LANGUAGE TypeApplications, DeriveGeneric #-}

module InfectionSource where

import Network.Wreq
import Data.Csv
import Data.Csv.Lens
import Data.Aeson
import Control.Lens
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

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

fetchInfectionSource = do
  response <- get nswHealthUrl
  let propertyMap = response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow ^. to makeMap
    in return propertyMap

-- | List of CSV Rows to {postcode: {date: {infection_source: number_of_patients}}}
makeMap :: [CsvRow] -> M.Map String (M.Map String (M.Map String Int))
makeMap = foldl (\acc -> \csv ->
                    (M.unionWith . M.unionWith  . M.unionWith $ (+))
                    (M.singleton (postcode csv) . M.singleton (notification_date csv) . M.singleton (likely_source_of_infection csv) $ 1)
                    acc
                ) M.empty
