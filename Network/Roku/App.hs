module Network.Roku.App where

import Control.Applicative
import Data.Text
import Text.XML.Light.Extractors

newtype AppId
  = AppId Text
  deriving Show

data AppType
  = Appl
  | Ssvr
  deriving Show

newtype AppVersion
  = AppVersion Text
  deriving Show

newtype AppName
  = AppName Text
  deriving Show

data App
  = App AppId AppType AppVersion AppName
  deriving Show

data AppOrHomescreen
  = NormalApp App
  | Homescreen
  deriving Show

data Screensaver
  = Screensaver App
  deriving Show

data ActiveAppResponse
  = ActiveAppResponse AppOrHomescreen (Maybe Screensaver)
  deriving Show

appBodyParser :: ElementExtractor App
appBodyParser = do
  let p = Right . pack
  i <- attribAs "id" p
  let t' "appl" = Right Appl
      t' "ssvr" = Right Ssvr
      t' t = Left t
  t <- attribAs "type" t'
  v <- attribAs "version" p
  n <- contents $ textAs p
  return $ App (AppId i) t (AppVersion v) (AppName n)

appsParser :: ContentsExtractor [App]
appsParser =
  element "apps" . children . many $ element "app" appBodyParser

activeAppParser :: ContentsExtractor ActiveAppResponse
activeAppParser =
  element "active-app" . children $ do
    app <- optional $ element "app" appBodyParser
    ssvr <- optional $ element "screensaver" appBodyParser
    return $ ActiveAppResponse (maybe Homescreen NormalApp app) $ Screensaver <$> ssvr
