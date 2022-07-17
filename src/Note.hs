{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Note where

import Protolude

import Data.Aeson
import Servant (FromHttpApiData, MimeRender)

newtype Tag = Tag Text
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Text

newtype NoteId = NoteId Text
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromHttpApiData) via Text

newtype AttachmentId = AttachmentId Text
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromHttpApiData) via Text

data Attachment = Attachment
  { attachId :: AttachmentId
  , attachFileName :: Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Attachment where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

data Note = Note
  { noteId :: NoteId
  , noteTitle :: Text
  , noteBody :: Text
  , noteTags :: Set Tag
  , noteAttachments :: Set Attachment
  } deriving (Eq, Show, Generic)

instance ToJSON Note where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }
