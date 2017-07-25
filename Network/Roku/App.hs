module Network.Roku.App where

import Data.Text

newtype AppId
  = AppId Text
  deriving Show

data AppType
  = Appl
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
