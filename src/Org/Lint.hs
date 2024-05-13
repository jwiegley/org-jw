{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.Lint where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Writer
import Data.Data
import Data.Hashable
import Data.List (nub)
import Data.Text.Lazy qualified as T
-- import Debug.Trace
import GHC.Generics
import Org.Data
import Org.Types

-- import Text.Show.Pretty

data LintMessageKind = LintError | LintWarning | LintInfo
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

data LintMessageCode
  = MisplacedProperty
  | MisplacedTimestamp
  | MisplacedLogEntry
  | DuplicatedProperty
  | TitleWithExcessiveWhitespace
  | TimestampsOnNonTodo
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

data LintMessage = LintMessage
  { lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode,
    lintMsgFile :: FilePath,
    lintMsgLine :: Int,
    lintMsgColumn :: Int
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

{-

Linting rules for org file collections:

-}

lintOrgData :: OrgData -> [LintMessage]
lintOrgData org = snd . runWriter $ mapM_ lintOrgFile (org ^. orgFiles)

{-

Linting rules for org files:

-}

lintOrgFile :: OrgFile -> Writer [LintMessage] ()
lintOrgFile org = mapM_ lintOrgEntry (org ^. fileEntries)

{-

Linting rules for org file entries:

- All TODO entries have ID properties, anything recent also has a CREATED
  property.

- No CREATED date lies in the future.

- No property is duplicated.

- No tag is duplicated.

- No title has internal whitespace other than single spaces.

- No title has special characters without escaping.

- Leading and trailing whitespace is consistent within log entries.

- There is no whitespace preceding the event log.

- There is no whitespace after the PROPERTY block (and/or event log) when
  there is no whitespace at the end of the entry.

- If an entry has trailing whitespace, it's siblings have the same whitespace.

- Property blocks are never empty.

- Only TODO items have SCHEDULED/DEADLINE/CLOSED timestamps.

- Don't use :SCRIPT:, use org-babel

-}

lintOrgEntry :: Entry -> Writer [LintMessage] ()
lintOrgEntry e = do
  checkFor LintError MisplacedProperty $
    any (":PROPERTIES:" `T.isInfixOf`) (e ^. entryText)
  checkFor LintError MisplacedTimestamp $
    any
      ( \t ->
          "SCHEDULED:" `T.isInfixOf` t
            || "DEADLINE:" `T.isInfixOf` t
            || "CLOSED:" `T.isInfixOf` t
      )
      (e ^. entryText)
  checkFor LintError MisplacedLogEntry $
    any
      ( \t ->
          "- State" `T.isInfixOf` t
            || "- Note taken" `T.isInfixOf` t
            || ":LOGBOOK:" `T.isInfixOf` t
      )
      (e ^. entryText)
  checkFor LintWarning TitleWithExcessiveWhitespace $
    "  " `T.isInfixOf` (e ^. entryTitle)
  checkFor LintError DuplicatedProperty $
    (e ^.. entryProperties . traverse . name)
      /= nub (e ^.. entryProperties . traverse . name)
  checkFor LintWarning TimestampsOnNonTodo $
    not (null (e ^. entryStamps))
      && case e ^? keyword of
        Nothing -> True
        Just kw -> not (isTodo kw)
  where
    checkFor ::
      LintMessageKind ->
      LintMessageCode ->
      Bool ->
      Writer [LintMessage] ()
    checkFor kind code b =
      when b $ do
        -- traceM $ "entry: " ++ ppShow e
        tell
          [ LintMessage
              kind
              code
              (e ^. entryFile)
              (e ^. entryLine)
              (e ^. entryColumn)
          ]

showLintOrg :: LintMessage -> String
showLintOrg (LintMessage kind code file line col) =
  file
    ++ ":"
    ++ show line
    ++ ":"
    ++ show col
    ++ ": "
    ++ renderKind kind
    ++ " "
    ++ renderCode code
  where
    renderKind = \case
      LintError -> "ERROR"
      LintWarning -> "WARN"
      LintInfo -> "INFO"
    renderCode = \case
      MisplacedProperty ->
        "Misplaced :PROPERTY: block"
      MisplacedTimestamp ->
        "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry ->
        "Misplaced state change, note or LOGBOOK"
      TitleWithExcessiveWhitespace ->
        "Title with excessive whitespace"
      DuplicatedProperty ->
        "Duplicated property"
      TimestampsOnNonTodo ->
        "Timestamps found on non-todo entry"
