{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.Lint where

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Control.Monad.Writer
import Data.Data
import Data.Foldable (forM_)
import Data.Hashable
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
-- import Debug.Trace (traceM)
import GHC.Generics hiding (to)
import Org.Data
import Org.Printer
import Org.Types
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string)

-- import Text.Show.Pretty

data LintMessageKind = LintInfo | LintWarn | LintError
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

parseLintMessageKind :: BasicParser LintMessageKind
parseLintMessageKind =
  (LintError <$ string "ERROR")
    <|> (LintWarn <$ string "WARN")
    <|> (LintInfo <$ string "INFO")

data LintMessageCode
  = MisplacedProperty Entry
  | MisplacedTimestamp Entry
  | MisplacedLogEntry Entry
  | MisplacedDrawerEnd Entry
  | DuplicatedProperties Entry
  | DuplicatedIdentifier Text [Entry]
  | TitleWithExcessiveWhitespace Entry
  | TimestampsOnNonTodo Entry
  | UnevenBodyWhitespace Entry
  | EmptyBodyWhitespace Entry
  | MultipleBlankLines Entry
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
  MisplacedDrawerEnd e -> MisplacedDrawerEnd <$> f e
  DuplicatedProperties e -> DuplicatedProperties <$> f e
  DuplicatedIdentifier _ [] -> error "impossible"
  DuplicatedIdentifier ident (e : es) ->
    DuplicatedIdentifier ident . (: es) <$> f e
  TitleWithExcessiveWhitespace e -> TitleWithExcessiveWhitespace <$> f e
  TimestampsOnNonTodo e -> TimestampsOnNonTodo <$> f e
  UnevenBodyWhitespace e -> UnevenBodyWhitespace <$> f e
  EmptyBodyWhitespace e -> EmptyBodyWhitespace <$> f e
  MultipleBlankLines e -> MultipleBlankLines <$> f e

{-

Linting rules for org file collections:

-}

lintOrgData :: String -> OrgData -> [LintMessage]
lintOrgData level org = snd . runWriter $ do
  let ids = foldAllEntries org M.empty $ \e m ->
        maybe
          m
          (\ident -> m & at ident %~ Just . maybe [e] (e :))
          (e ^? entryId)

  forM_ (M.assocs ids) $ \(k, es) ->
    when (length es > 1) $
      tell [LintMessage LintError (DuplicatedIdentifier k es)]

  let level' =
        fromMaybe
          LintInfo
          (parseMaybe parseLintMessageKind (T.pack level))

  mapM_ (lintOrgFile level') (org ^. orgFiles)

{-

Linting rules for org files:

-}

lintOrgFile :: LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile level org =
  mapM_
    (lintOrgEntry level)
    ( org
        ^.. entries
          ( org ^. fileHeader . headerPropertiesDrawer
              ++ org ^. fileHeader . headerFileProperties
          )
    )

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

lintOrgEntry :: LintMessageKind -> Entry -> Writer [LintMessage] ()
lintOrgEntry level e = do
  checkFor LintError (MisplacedProperty e) $
    any ((":properties:" `T.isInfixOf`) . T.toLower) bodyText
  checkFor LintError (MisplacedTimestamp e) $
    any
      ( \t ->
          "SCHEDULED:" `T.isInfixOf` t
            || "DEADLINE:" `T.isInfixOf` t
            || "CLOSED:" `T.isInfixOf` t
      )
      bodyText
  checkFor LintError (MisplacedLogEntry e) $
    any
      ( \t ->
          "- State " `T.isInfixOf` t
            || "- Note taken " `T.isInfixOf` t
            || ":logbook:" `T.isInfixOf` T.toLower t
      )
      bodyText
  checkFor LintError (MisplacedDrawerEnd e) $
    any
      ( ( \t ->
            ":end:" `T.isInfixOf` t
              || "#+end" `T.isInfixOf` t
        )
          . T.toLower
      )
      bodyText
  checkFor LintWarn (TitleWithExcessiveWhitespace e) $
    "  " `T.isInfixOf` (e ^. entryTitle)
  checkFor LintError (DuplicatedProperties e) $
    (e ^.. entryProperties . traverse . name)
      /= nub (e ^.. entryProperties . traverse . name)
  checkFor LintWarn (TimestampsOnNonTodo e) $
    not (null (e ^. entryStamps))
      && case e ^? keyword of
        Nothing -> True
        Just kw -> not (isTodo kw)
  -- jww (2024-05-14): Need to check log entries also
  checkFor LintInfo (UnevenBodyWhitespace e) $
    e ^. entryText . leadSpace /= e ^. entryText . endSpace
  -- jww (2024-05-14): Need to check log entries also
  checkFor LintInfo (EmptyBodyWhitespace e) $
    not (null bodyText)
      && e ^. entryText . leadSpace == T.unlines bodyText
  -- jww (2024-05-14): Need to check in log entries also
  checkFor
    LintInfo
    (MultipleBlankLines e)
    ( any ((> 1) . length . T.lines) $
        e ^.. entryText . blocks . traverse . _Whitespace
    )
  where
    bodyText =
      e
        ^. entryText
          . blocks
          . traverse
          . filtered (hasn't _Drawer)
          . to (showBlock "")
        ++ e
          ^. entryLogEntries
            . traverse
            . failing (_LogState . _4) (_LogNote . _2)
            . _Just
            . blocks
            . traverse
            . filtered (hasn't _Drawer)
            . to (showBlock "")

    checkFor ::
      LintMessageKind ->
      LintMessageCode ->
      Bool ->
      Writer [LintMessage] ()
    checkFor kind code b =
      when (b && kind >= level) $ do
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
      LintWarn -> "WARN"
      LintInfo -> "INFO"
    renderCode = case code of
      MisplacedProperty _e ->
        "Misplaced :PROPERTIES: block"
      MisplacedTimestamp _e ->
        "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry _e ->
        "Misplaced state change, note or LOGBOOK"
      MisplacedDrawerEnd _e ->
        "Misplaced end of drawer"
      TitleWithExcessiveWhitespace _e ->
        "Title with excessive whitespace"
      DuplicatedProperties _e ->
        "Duplicated properties"
      DuplicatedIdentifier ident _entries ->
        "Duplicated identifier " ++ T.unpack ident
      TimestampsOnNonTodo _e ->
        "Timestamps found on non-todo entry"
      UnevenBodyWhitespace _e ->
        "Whitespace surrounding body is not even"
      EmptyBodyWhitespace _e ->
        "Whitespace only body"
      MultipleBlankLines _e ->
        "Multiple blank lines"
