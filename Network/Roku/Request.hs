{-# LANGUAGE OverloadedStrings #-}
module Network.Roku.Request where

import Data.ByteString
import Data.Semigroup
import Data.Text.Encoding
import Network.Roku.App
import Network.Roku.Keys
import qualified Network.HTTP.Client as HTTP

data Query
  = Apps
  | ActiveApp
  | Icon AppId
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
  | Launch AppId
  | Install AppId
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
          Icon (AppId appId) -> "icon/" <> encodeUtf8 appId
resource (Keyup key) =
  Resource Post $ "keyup/" <> keyValue key
resource (Keydown key) =
  Resource Post $ "keydown/" <> keyValue key
resource (Keypress key) =
  Resource Post $ "keypress/" <> keyValue key
resource (Launch (AppId appId)) =
  Resource Post $ "launch/" <> encodeUtf8 appId
resource (Install (AppId appId)) =
  Resource Post $ "install/" <> encodeUtf8 appId

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
