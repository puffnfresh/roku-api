{-# LANGUAGE OverloadedStrings #-}
module Network.Roku.Request where

import Data.ByteString
import Data.Semigroup
import Network.Roku.Keys
import qualified Network.HTTP.Client as HTTP

data Query
  = Apps
  | ActiveApp
  deriving Show

data Method
  = Get
  | Post
  deriving Show

data Request
  = Query Query
  | Keyup Key
  | Keydown Key
  | Keypress Key
  deriving Show

data Resource
  = Resource Method ByteString
  deriving Show

resource :: Request -> Resource
resource (Query query) =
  Resource Get $ "query/" <> path
  where path = case query of
          Apps -> "apps"
          ActiveApp -> "active-app"
resource (Keyup key) =
  Resource Post $ "keyup/" <> keyValue key
resource (Keydown key) =
  Resource Post $ "keydown/" <> keyValue key
resource (Keypress key) =
  Resource Post $ "keypress/" <> keyValue key

request :: Request -> ByteString -> HTTP.Request
request r host =
  HTTP.defaultRequest { HTTP.host = host
                      , HTTP.method = method'
                      , HTTP.path = path
                      , HTTP.port = 8060
                      }
  where Resource method path = resource r
        method' = case method of
          Post -> "POST"
          Get -> "GET"
