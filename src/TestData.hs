{-# LANGUAGE TypeApplications, DeriveGeneric #-}

module TestData where

import Network.Wreq
import Data.Csv
import Data.Csv.Lens
import Data.Aeson
import Control.Lens
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

nswHealthUrl :: String
nswHealthUrl = "https://data.nsw.gov.au/data/dataset/60616720-3c60-4c52-b499-751f31e3b132/resource/945c6204-272a-4cad-8e33-dde791f5059a/download/covid-19-tests-by-date-and-postcode-local-health-district-and-local-government-area.csv"

data CsvRow = CsvRow
  {
    postcode :: String
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow
instance ToJSON CsvRow

data TestResult = TestResult
  { tests :: Int
  } deriving (Show)

tester = fetchTestData >>= print

fetchTestData = do
  response <- get nswHealthUrl
  let propertyMap = response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow ^. to makeMap
    in return propertyMap

-- | List of CSV Rows to {postcode: number of tests }
makeMap :: [CsvRow] -> M.Map String Int
makeMap = foldl (\acc -> \csv ->
                    (M.unionWith (+))
                    (M.singleton (case postcode csv of
                                    "" -> "0"
                                    _ -> postcode csv)
                    1)
                    acc
                ) M.empty
