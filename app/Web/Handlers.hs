{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Handlers
    ( getAllNotes
    , getNoteById
    , getAttachmentDataById
    ) where

import Protolude hiding (Handler)

import Data.Coerce (coerce)
import qualified Data.Set as Set
import Web.Note
import qualified Note as Domain
import qualified Database as DB
import Servant

domainNoteToWebNote :: Domain.Note -> Note
domainNoteToWebNote Domain.Note{..} =
  let Domain.NoteId noteId' = noteId
      noteLinks = Links (Link $ "http://localhost:8081/note/" <> noteId')
      toWebAttachment Domain.Attachment{..} =
        let Domain.AttachmentId attachId' = attachId
        in Attachment attachFileName (Links (Link $ "http://localhost:8081/file/" <> attachId'))
      webAttachments = noteAttachments
        & Set.toList
        <&> toWebAttachment
      embedded = Embedded webAttachments
  in Note noteTitle noteBody (Set.toList noteTags <&> coerce) noteLinks embedded

or404 :: Maybe a -> Handler a
or404 Nothing = throwError err404
or404 (Just x) = pure x

getAllNotes :: Handler [Note]
getAllNotes = do
  domainNotes <- liftIO DB.allNotes
  pure $ domainNoteToWebNote <$> domainNotes

getNoteById :: NoteId -> Handler Note
getNoteById noteId = do
  liftIO (DB.getNoteById (coerce noteId)) >>= or404 <&> domainNoteToWebNote

getAttachmentDataById :: AttachmentId -> Handler ByteString
getAttachmentDataById attachmentId' = do
  liftIO (DB.getAttachmentDataById (coerce attachmentId')) >>= or404
