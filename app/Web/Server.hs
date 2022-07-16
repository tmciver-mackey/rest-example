{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds       #-}

module Web.Server (app) where

import Protolude

import Network.Wai
import Servant
import Web.API
import Web.Handlers

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
       allNotes
  :<|> getNoteById
