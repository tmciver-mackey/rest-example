{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( allNotes
  , getNoteById
  , getAttachmentDataById
  ) where

import Protolude

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Note
import Servant
import System.FilePath.Posix (takeFileName)

allNotes :: IO [Note]
allNotes = pure $ Map.elems notes

getNoteById :: NoteId -> IO (Maybe Note)
getNoteById noteId' = pure $ Map.lookup noteId' notes

fileMap :: Map AttachmentId FilePath
fileMap = Map.fromList
  [ (AttachmentId "sdflkjs", "files/dummy.pdf")
  , (AttachmentId "woidclm", "files/simple.txt")
  , (AttachmentId "wqowok", "files/dummy.pdf")
  , (AttachmentId "apoijmnsoi", "files/lambda.jpeg")
  ]

getAttachmentDataById :: AttachmentId -> IO (Maybe ByteString)
getAttachmentDataById attachmentId' =
  case Map.lookup attachmentId' fileMap of
    Nothing -> pure Nothing
    Just filePath -> BS.readFile filePath <&> Just

attachmentFromId :: AttachmentId -> Attachment
attachmentFromId attachId =
  let fileName = T.pack . takeFileName . fromJust $ Map.lookup attachId fileMap
  in Attachment attachId fileName

notes :: Map NoteId Note
notes = Map.fromList
  [ (NoteId "abc", Note (NoteId "abc")
                        "My first note"
                        "Lorem Ipsum"
                        (Set.fromList [Tag "IBM", Tag "AAPL"])
                        (Set.fromList $ attachmentFromId <$> [AttachmentId "sdflkjs", AttachmentId "woidclm"])
    )
  , (NoteId "123", Note (NoteId "123")
                        "My second note"
                        "Lorem Ipsum"
                        Set.empty
                        (Set.fromList $ attachmentFromId <$> [AttachmentId "wqowok", AttachmentId "apoijmnsoi"])
    )
  ]

