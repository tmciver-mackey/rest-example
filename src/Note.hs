{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}

module Note where

import Protolude

import Data.Aeson
import Servant (FromHttpApiData, MimeRender)
import qualified Data.Set as Set

newtype Tag = Tag Text
  deriving (Eq, Ord, Show)

newtype NoteId = NoteId Text
  deriving (Eq, Ord, Show)

newtype AttachmentId = AttachmentId Text
  deriving (Eq, Ord, Show)

data Attachment = Attachment
  { attachId :: AttachmentId
  , attachFileName :: Text
  } deriving (Eq, Ord, Show)

data Note = Note
  { noteId :: NoteId
  , noteTitle :: Text
  , noteBody :: Text
  , noteTags :: Set Tag
  , noteAttachments :: Set Attachment
  } deriving (Eq, Show)
