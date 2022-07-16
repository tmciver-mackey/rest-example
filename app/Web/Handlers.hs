{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Handlers
    ( allNotes
    , getNoteById
    ) where

import Protolude hiding (Handler)

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

notes :: Map NoteId Note
notes = Map.fromList
  [ (NoteId "abc", Note (NoteId "abc") "My first note" "Lorem Ipsum" (Set.fromList [Tag "IBM", Tag "AAPL"]))
  , (NoteId "123", Note (NoteId "123") "My second note" "Lorem Ipsum" Set.empty)
  ]
