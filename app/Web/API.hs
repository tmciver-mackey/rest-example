{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.API where

import Protolude

import Note (Note)
import Servant

type API =
     "notes"
  :> Get '[JSON] [Note]
