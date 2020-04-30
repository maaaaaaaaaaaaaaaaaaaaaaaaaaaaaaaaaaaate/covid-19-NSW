{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GeoMultiToPoint where

import Data.Aeson
import Data.Geospatial
import qualified Data.Sequence as S
import Data.LinearRing
import Data.Foldable (toList)

import GHC.Generics

data Properties = Properties {
  postcode' :: String,
  area' :: Double
  } deriving (Show, Generic)
instance ToJSON Properties where
instance FromJSON Properties where
    parseJSON = withObject "Properties" $ \v -> Properties
        <$> v .: "POA_CODE16"
        <*> v .: "AREASQKM16"

postcodeGeoPoint :: IO ()
postcodeGeoPoint = do
  decodeFileStrict "postcode_multipoint.json" >>= return . fmap (
    \(GeoFeatureCollection box geo) -> GeoFeatureCollection box (fmap makePoint $ geo)
    )
    >>= \m -> case m of
                Just json -> encodeFile "postcode_point.json" json
                Nothing -> print "nada"


makePoint :: GeoFeature Properties -> GeoFeature Properties
makePoint (GeoFeature bb geo props fid) = GeoFeature bb newGeo props fid
  where newGeo = let MultiPolygon (GeoMultiPolygon seqs) = geo
                 in Point (GeoPoint $ remakePoint (average (makeList seqs)))

remakePoint (x,y) = GeoPointXY (PointXY x y)

makeList seqs = concat . concat . map (map (toList . toSeq)) . map toList . toList $ seqs

average xs = foldl geoAdd (0,0) xs `geodiv` fromIntegral (length xs)

geoAdd (a,b) (GeoPointXY (PointXY x y)) = (a+x, b+y)

geodiv (a,b) d = (a/d, b/d)
