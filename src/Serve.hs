{-# LANGUAGE OverloadedStrings #-}

module Serve where

-- serve geo json

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy.Char8 as BS

import Persist (getGeoJson)

startServer = getGeoJson >>= BS.writeFile "geo.json"

app request response = liftIO getGeoJson >>=
  response . responseLBS status200
    [ ("Content-Type", "application/json")
    , ("Access-Control-Allow-Origin","*")]
