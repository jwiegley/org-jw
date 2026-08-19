{-# LANGUAGE OverloadedStrings #-}

{- | Pure JSON-line renderers for CLI output, kept in the library so they
are testable. The executable's per-row IO printers wrap these.
-}
module Org.DB.Render (
  entryRowJson,
  jsonEscapeText,
) where

import Data.Char (isControl, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Org.DB.Types
import Text.Printf (printf)

{- | One JSON-lines object for @db query --format json@. The optional file
path is the files-table path for the row's file_id; when unknown the
field is omitted rather than emitted as null.
-}
entryRowJson :: Maybe Text -> EntryRow -> Text
entryRowJson mfile row =
  "{\"id\":\""
    <> jsonEscapeText (erId row)
    <> "\",\"title\":\""
    <> jsonEscapeText (erTitle row)
    <> "\",\"keyword\":"
    <> maybe "null" (\k -> "\"" <> jsonEscapeText k <> "\"") (erKeywordValue row)
    <> ",\"depth\":"
    <> T.pack (show (erDepth row))
    <> maybe "" (\f -> ",\"file\":\"" <> jsonEscapeText f <> "\"") mfile
    <> "}"

jsonEscapeText :: Text -> Text
jsonEscapeText = T.concatMap esc
 where
  esc '"' = "\\\""
  esc '\\' = "\\\\"
  esc '\n' = "\\n"
  esc '\r' = "\\r"
  esc '\t' = "\\t"
  esc c
    | isControl c = T.pack (printf "\\u%04x" (ord c))
    | otherwise = T.singleton c
