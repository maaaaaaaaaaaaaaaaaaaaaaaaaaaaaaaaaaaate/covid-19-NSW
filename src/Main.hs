{-# LANGUAGE OverloadedStrings #-}

module Main where

import Serve (startServer)
import InfectionSource (fetchInfectionSource, likely_source_of_infection)
import GeoJson (testGetLoc, lga_name19)
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = startServer

--   do
--   infections <- fetchInfectionSource
--   locations <- testGetLoc
--   let diffNum = length locations - length infections
--   BS.putStrLn . encode
-- --    . filter (\v -> v == "Locally acquired - contact not identified" || v == "Under investigation" . likely_source_of_infection . fst)
-- --    . filter ((==) "Sydney (C)" . lga_name19 . snd)
--     . foldl (\acc -> \(inf, loc) -> case Map.lookup (lga_name19 loc) acc of
--                 Just v ->  Map.insert (lga_name19 loc) (Map.insertWith (+) (likely_source_of_infection inf) 1 v) acc
--                 Nothing -> Map.insert (lga_name19 loc) (Map.singleton (likely_source_of_infection inf) (1 :: Int)) acc
--             ) Map.empty
--     . zip infections $ drop diffNum locations
