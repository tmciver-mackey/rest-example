{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.API where

import Note
import Servant

type API =
     "notes"
  :> Get '[JSON] [Note]

  :<|> "note"
  :> Capture "noteId" NoteId
  :> Get '[JSON] Note
