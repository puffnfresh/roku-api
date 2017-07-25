{-# LANGUAGE OverloadedStrings #-}
module Network.Roku.Request
  ( Query(..)
  , Method(..)
  , Request(..)
  , Resource(..)
  , request
  , resource
  , QueryError(..)
  , queryActiveApp
  , queryApps
  )
where

import Data.ByteString
import Data.Semigroup
import Data.Text.Encoding
import Data.Bifunctor
import Network.Roku.App
import Text.XML.Light.Extractors
import Text.XML.Light.Input
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP
import qualified Network.Roku.Keys as Keys

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
  | Keyup Keys.Key
  | Keydown Keys.Key
  | Keypress Keys.Key
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
  Resource Post $ "keyup/" <> Keys.keyValue key
resource (Keydown key) =
  Resource Post $ "keydown/" <> Keys.keyValue key
resource (Keypress key) =
  Resource Post $ "keypress/" <> Keys.keyValue key
resource (Launch (AppId appId)) =
  Resource Post $ "launch/" <> encodeUtf8 appId
resource (Install (AppId appId)) =
  Resource Post $ "install/" <> encodeUtf8 appId

data QueryError
  = QueryInvalidXML LBS.ByteString
  | QueryExtractionErr ExtractionErr
  deriving Show

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

runQuery :: Query -> ContentsExtractor a -> ByteString -> HTTP.Manager -> IO (Either QueryError a)
runQuery q p host manager = do
  r <- HTTP.httpLbs (request (Query q) host) manager
  let body = HTTP.responseBody r
  return $ do
    doc <- maybe (Left $ QueryInvalidXML body) Right $ parseXMLDoc body
    first QueryExtractionErr $ extractDocContents p doc

queryActiveApp :: ByteString -> HTTP.Manager -> IO (Either QueryError ActiveAppResponse)
queryActiveApp = do
  runQuery ActiveApp activeAppParser

queryApps :: ByteString -> HTTP.Manager -> IO (Either QueryError [App])
queryApps = do
  runQuery Apps appsParser
