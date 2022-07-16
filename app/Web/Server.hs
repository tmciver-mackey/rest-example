{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Web.Server (app) where

import Protolude

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Set as Set
import Network.Wai

import Servant

import Web.API
import Note

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = pure notes

notes :: [Note]
notes = [ Note (NoteId "abc") "My first note" "Lorem Ipsum" (Set.fromList [Tag "IBM", Tag "AAPL"])
        , Note (NoteId "123") "My second note" "Lorem Ipsum" Set.empty
        ]
