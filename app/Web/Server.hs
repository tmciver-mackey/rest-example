{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Web.Server (app) where

import Protolude

import Data.Aeson
-- import Data.Aeson.TH
import Network.Wai

import Servant

import Web.API
import Web.Handlers
import Note

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
       allNotes
  :<|> getNoteById
