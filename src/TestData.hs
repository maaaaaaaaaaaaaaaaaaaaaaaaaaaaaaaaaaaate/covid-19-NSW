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
nswHealthUrl = "https://data.nsw.gov.au/data/dataset/5424aa3b-550d-4637-ae50-7f458ce327f4/resource/227f6b65-025c-482c-9f22-a25cf1b8594f/download/covid-19-tests-by-date-and-location-and-result.csv"

data CsvRow = CsvRow
  {
    postcode :: String,
    result :: String
  } deriving (Show, Generic)
instance FromNamedRecord CsvRow
instance ToNamedRecord CsvRow
instance ToJSON CsvRow

data TestResult = TestResult
  { cases :: Int
  , tests :: Int
  } deriving (Show)

tester =  fetchTestData >>= print

fetchTestData = do
  response <- get nswHealthUrl
  let propertyMap = response ^. responseBody ^.. namedCsv . rows . _NamedRecord @CsvRow ^. to makeMap
    in return propertyMap

-- | List of CSV Rows to {postcode: {cases OR tests : number}}
makeMap :: [CsvRow] -> M.Map String (M.Map String Int)
makeMap = foldl (\acc -> \csv ->
                    (M.unionWith . M.unionWith $ (+))
                    (M.singleton (case postcode csv of
                                    "" -> "0"
                                    _ -> postcode csv)
                    (case result csv of
                       "Case - Confirmed" -> M.insert "cases" 1 $ M.singleton "tests" 1
                       "Tested & excluded" -> M.insert "cases" 0 $ M.singleton "tests" 1))
                    acc
                ) M.empty
