{-# LANGUAGE OverloadedStrings #-}
module Network.Roku.Keys where

import Prelude hiding (Either(..))
import Data.ByteString.Char8

data Key
  = Home
  | Rev
  | Fwd
  | Play
  | Select
  | Left
  | Right
  | Down
  | Up
  | Back
  | InstantReplay
  | Info
  | Backspace
  | Search
  | Enter
  | Lit Char
  deriving Show

keyValue :: Key -> ByteString
keyValue Home =
  "Home"
keyValue Rev =
  "Rev"
keyValue Fwd =
  "Fwd"
keyValue Play =
  "Play"
keyValue Select =
  "Select"
keyValue Left =
  "Left"
keyValue Right =
  "Right"
keyValue Down =
  "Down"
keyValue Up =
  "Up"
keyValue Back =
  "Back"
keyValue InstantReplay =
  "InstantReplay"
keyValue Info =
  "Info"
keyValue Backspace =
  "Backspace"
keyValue Search =
  "Search"
keyValue Enter =
  "Enter"
keyValue (Lit c) =
  snoc "Lit_" c
