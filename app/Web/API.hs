{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.API where

import Protolude

import Web.Note
import Servant

type API =
     "notes"
  :> Get '[JSON, NoteHAL] [Note]

  :<|> "note"
  :> Capture "noteId" NoteId
  :> Get '[JSON, NoteHAL] Note

  :<|> "file"
  :> Capture "attachmentId" AttachmentId
  :> Get '[OctetStream] ByteString
