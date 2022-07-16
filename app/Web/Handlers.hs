{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Handlers
    ( allNotes
    , getNoteById
    , getAttachmentById
    ) where

import Protolude hiding (Handler)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Note
import Servant

allNotes :: Handler [Note]
allNotes = pure $ Map.elems notes

getNoteById :: NoteId -> Handler Note
getNoteById noteId' =
  case Map.lookup noteId' notes of
    Nothing -> throwError err404
    Just n -> pure n

getAttachmentById :: AttachmentId -> Handler ByteString
getAttachmentById attachmentId' = do
  let fileMap :: Map AttachmentId FilePath
      fileMap = Map.fromList
        [ (AttachmentId "sdflkjs", "files/dummy.pdf")
        , (AttachmentId "woidclm", "files/simple.txt")
        , (AttachmentId "wqowok", "files/dummy.pdf")
        , (AttachmentId "apoijmnsoi", "files/lambda.jpeg")
        ]
  case Map.lookup attachmentId' fileMap of
    Nothing -> throwError err404
    Just filePath -> liftIO $ BS.readFile filePath

notes :: Map NoteId Note
notes = Map.fromList
  [ (NoteId "abc", Note (NoteId "abc")
                        "My first note"
                        "Lorem Ipsum"
                        (Set.fromList [Tag "IBM", Tag "AAPL"])
                        (Set.fromList [AttachmentId "sdflkjs", AttachmentId "woidclm"])
    )
  , (NoteId "123", Note (NoteId "123")
                        "My second note"
                        "Lorem Ipsum"
                        Set.empty
                        (Set.fromList [AttachmentId "wqowok", AttachmentId "apoijmnsoi"])
    )
  ]
