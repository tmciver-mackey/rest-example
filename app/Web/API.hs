{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.API where

import Protolude

import Note
import Servant

type API =
     "notes"
  :> Get '[JSON] [Note]

  :<|> "note"
  :> Capture "noteId" NoteId
  :> Get '[JSON] Note

  :<|> "file"
  :> Capture "attachmentId" AttachmentId
  :> Get '[OctetStream] ByteString
