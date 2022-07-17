{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Note where

import Protolude

import Data.Aeson
import Servant (FromHttpApiData, MimeRender)
import qualified Data.Set as Set

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
  toJSON Attachment{..} = object
    [ "fileName" .= attachFileName
    , "_links" .= object
      [ "_self" .= let AttachmentId attId = attachId
                   in "http://localhost:8081/file/" <> attId
      ]
    ]

data Note = Note
  { noteId :: NoteId
  , noteTitle :: Text
  , noteBody :: Text
  , noteTags :: Set Tag
  , noteAttachments :: Set Attachment
  } deriving (Eq, Show, Generic)

instance ToJSON Note where
  toJSON Note{..} = object
    [ "title" .= noteTitle
    , "body" .= noteBody
    , "tags" .= toJSON noteTags
    , "_links" .= object
      [ "_self" .= let NoteId nId = noteId
                   in "http://localhost:8081/note/" <> nId
      ]
    , "_embedded" .= object
      [ "attachments" .=
        (toJSON <$> Set.toList noteAttachments)
      ]
    ]
