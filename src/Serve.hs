{-# LANGUAGE OverloadedStrings #-}

module Serve where

-- serve geo json

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class

import Persist (getGeoJson)

startServer = run 3210 app

app request response = liftIO getGeoJson >>=
  response . responseLBS status200
    [ ("Content-Type", "application/json")
    , ("Access-Control-Allow-Origin","*")]
