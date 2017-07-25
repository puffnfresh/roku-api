module Network.Roku.Query where

import Data.Text

newtype AppId
  = AppId Text

data AppType
  = Appl

newtype AppVersion
  = AppVersion Text

newtype AppName
  = AppName Text

data App
  = App AppId AppType AppVersion AppName
