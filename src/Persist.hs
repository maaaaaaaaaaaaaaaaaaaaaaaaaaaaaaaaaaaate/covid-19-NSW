module Persist where

import qualified Data.ByteString.Lazy.Char8 as BS

-- TODO acid state persistence

import GeoJson (encodeGeoJson)

getGeoJson :: IO BS.ByteString
getGeoJson = encodeGeoJson
