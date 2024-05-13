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
-- import Debug.Trace

import Data.Foldable (forM_)
import Data.Hashable
import Data.List (nub)
import Data.Map qualified as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import GHC.Generics
import Org.Data
import Org.Types

-- import Text.Show.Pretty

data LintMessageKind = LintError | LintWarning | LintInfo
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

data LintMessageCode
  = MisplacedProperty Entry
  | MisplacedTimestamp Entry
  | MisplacedLogEntry Entry
  | DuplicatedProperties Entry
  | DuplicatedIdentifier Text [Entry]
  | TitleWithExcessiveWhitespace Entry
  | TimestampsOnNonTodo Entry
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

data LintMessage = LintMessage
  { lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

lintCodeEntry :: Lens' LintMessageCode Entry
lintCodeEntry f = \case
  MisplacedProperty e -> MisplacedProperty <$> f e
  MisplacedTimestamp e -> MisplacedTimestamp <$> f e
  MisplacedLogEntry e -> MisplacedLogEntry <$> f e
  DuplicatedProperties e -> DuplicatedProperties <$> f e
  DuplicatedIdentifier _ [] -> error "impossible"
  DuplicatedIdentifier ident (e : es) ->
    DuplicatedIdentifier ident . (: es) <$> f e
  TitleWithExcessiveWhitespace e -> TitleWithExcessiveWhitespace <$> f e
  TimestampsOnNonTodo e -> TimestampsOnNonTodo <$> f e

{-

Linting rules for org file collections:

-}

lintOrgData :: OrgData -> [LintMessage]
lintOrgData org = snd . runWriter $ do
  let ids = foldAllEntries org M.empty $ \e m ->
        maybe
          m
          (\ident -> m & at ident %~ Just . maybe [e] (e :))
          (e ^? entryId)
  forM_ (M.assocs ids) $ \(k, es) ->
    when (length es > 1) $
      tell [LintMessage LintError (DuplicatedIdentifier k es)]

  mapM_ lintOrgFile (org ^. orgFiles)

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
  checkFor LintError (MisplacedProperty e) $
    any (":PROPERTIES:" `T.isInfixOf`) (e ^. entryText)
  checkFor LintError (MisplacedTimestamp e) $
    any
      ( \t ->
          "SCHEDULED:" `T.isInfixOf` t
            || "DEADLINE:" `T.isInfixOf` t
            || "CLOSED:" `T.isInfixOf` t
      )
      (e ^. entryText)
  checkFor LintError (MisplacedLogEntry e) $
    any
      ( \t ->
          "- State" `T.isInfixOf` t
            || "- Note taken" `T.isInfixOf` t
            || ":LOGBOOK:" `T.isInfixOf` t
      )
      (e ^. entryText)
  checkFor LintWarning (TitleWithExcessiveWhitespace e) $
    "  " `T.isInfixOf` (e ^. entryTitle)
  checkFor LintError (DuplicatedProperties e) $
    (e ^.. entryProperties . traverse . name)
      /= nub (e ^.. entryProperties . traverse . name)
  checkFor LintWarning (TimestampsOnNonTodo e) $
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
        tell [LintMessage kind code]

showLintOrg :: LintMessage -> String
showLintOrg (LintMessage kind code) =
  file
    ++ ":"
    ++ show line
    ++ ":"
    ++ show col
    ++ ": "
    ++ renderKind
    ++ " "
    ++ renderCode
  where
    (file, line, col) =
      let e = code ^. lintCodeEntry
       in (e ^. entryFile, e ^. entryLine, e ^. entryColumn)
    renderKind = case kind of
      LintError -> "ERROR"
      LintWarning -> "WARN"
      LintInfo -> "INFO"
    renderCode = case code of
      MisplacedProperty _e ->
        "Misplaced :PROPERTY: block"
      MisplacedTimestamp _e ->
        "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry _e ->
        "Misplaced state change, note or LOGBOOK"
      TitleWithExcessiveWhitespace _e ->
        "Title with excessive whitespace"
      DuplicatedProperties _e ->
        "Duplicated properties"
      DuplicatedIdentifier ident _entries ->
        "Duplicated identifier " ++ T.unpack ident
      TimestampsOnNonTodo _e ->
        "Timestamps found on non-todo entry"
