{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Note where

import Protolude

import Data.Aeson

newtype Tag = Tag Text
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via Text

newtype NoteId = NoteId Text
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via Text

data Note = Note
  { noteId :: NoteId
  , noteTitle :: Text
  , noteBody :: Text
  , noteTags :: Set Tag
  } deriving (Eq, Show, Generic)

instance ToJSON Note where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }
