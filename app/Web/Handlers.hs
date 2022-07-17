{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Handlers
    ( allNotes
    , getNoteById
    , getAttachmentDataById
    ) where

import Protolude hiding (Handler)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Note
import Servant
import System.FilePath.Posix (takeFileName)

allNotes :: Handler [Note]
allNotes = pure $ Map.elems notes

getNoteById :: NoteId -> Handler Note
getNoteById noteId' =
  case Map.lookup noteId' notes of
    Nothing -> throwError err404
    Just n -> pure n

fileMap :: Map AttachmentId FilePath
fileMap = Map.fromList
  [ (AttachmentId "sdflkjs", "files/dummy.pdf")
  , (AttachmentId "woidclm", "files/simple.txt")
  , (AttachmentId "wqowok", "files/dummy.pdf")
  , (AttachmentId "apoijmnsoi", "files/lambda.jpeg")
  ]

getAttachmentDataById :: AttachmentId -> Handler ByteString
getAttachmentDataById attachmentId' =
  case Map.lookup attachmentId' fileMap of
    Nothing -> throwError err404
    Just filePath -> liftIO $ BS.readFile filePath

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
