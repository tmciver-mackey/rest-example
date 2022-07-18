{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Web.Note where

import Protolude

import Data.Aeson
import Servant (FromHttpApiData)

newtype AttachmentId = AttachmentId Text
  deriving (Eq, Ord, Show)
  deriving (FromHttpApiData) via Text

newtype NoteId = NoteId Text
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromHttpApiData) via Text

newtype Link = Link { href :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON Link

newtype Links = Links
  { _self :: Link }
  deriving (Eq, Show, Generic)

instance ToJSON Links

data Attachment = Attachment
  { attachFileName :: Text
  , attach_links :: Links
  } deriving (Eq, Show, Generic)

instance ToJSON Attachment where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

newtype Embedded = Embedded
  { embeddedAttachments :: [Attachment]
  } deriving (Eq, Show, Generic)

instance ToJSON Embedded where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

data Note = Note
  { noteTitle :: Text
  , noteBody :: Text
  , noteTags :: [Text]
  , note_links :: Links
  , note_embedded :: Embedded
  } deriving (Eq, Show, Generic)

instance ToJSON Note where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }
