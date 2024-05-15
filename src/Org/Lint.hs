{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.Lint where

import Control.Applicative
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Writer
import Data.Data
import Data.Foldable (forM_)
import Data.Hashable
import Data.List (intercalate, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Debug.Trace (traceM)
import GHC.Generics hiding (to)
import Org.Data
import Org.Printer
import Org.Types
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string)
import Text.Show.Pretty

data LintMessageKind = LintDebug | LintInfo | LintWarn | LintError
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Hashable)

parseLintMessageKind :: BasicParser LintMessageKind
parseLintMessageKind =
  (LintError <$ string "ERROR")
    <|> (LintWarn <$ string "WARN")
    <|> (LintInfo <$ string "INFO")
    <|> (LintDebug <$ string "DEBUG")

data LintMessageCode
  = MisplacedProperty Entry
  | MisplacedTimestamp Entry
  | MisplacedLogEntry Entry
  | MisplacedDrawerEnd Entry
  | DuplicatedFileProperties OrgFile
  | DuplicatedProperties Entry
  | DuplicatedIdentifier Text (NonEmpty Entry)
  | MultipleLogbooks Entry
  | MixedLogbooks Entry
  | TitleWithExcessiveWhitespace Entry
  | TimestampsOnNonTodo Entry
  | UnevenBodyWhitespace Entry
  | UnevenFilePreambleWhitespace OrgFile
  | EmptyBodyWhitespace Entry
  | MultipleBlankLines Entry
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

data LintMessage = LintMessage
  { lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

{-

Linting rules for org file collections:

-}

lintOrgData :: String -> OrgData -> [LintMessage]
lintOrgData level org = snd . runWriter $ do
  let ids = foldAllEntries org M.empty $ \e m ->
        maybe
          m
          ( \ident ->
              m
                & at ident %~ Just . maybe (NE.singleton e) (NE.cons e)
          )
          (e ^? entryId)

  forM_ (M.assocs ids) $ \(k, es) ->
    when (NE.length es > 1) $
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
lintOrgFile level org = do
  checkFor LintError (DuplicatedFileProperties org) $
    sort (props ^.. traverse . name)
      /= nub (sort (props ^.. traverse . name))
  -- checkFor LintInfo (UnevenFilePreambleWhitespace org) $
  --   org ^? fileHeader . headerPreamble . leadSpace
  --     /= org ^? fileHeader . headerPreamble . endSpace
  case reverse (org ^.. allEntries) of
    [] -> pure ()
    e : es -> do
      mapM_ (lintOrgEntry False level) (reverse es)
      lintOrgEntry True level e
  where
    props =
      org ^. fileHeader . headerPropertiesDrawer
        ++ org ^. fileHeader . headerFileProperties
    checkFor ::
      LintMessageKind ->
      LintMessageCode ->
      Bool ->
      Writer [LintMessage] ()
    checkFor kind code b =
      when (b && kind >= level) $ do
        when (level == LintDebug) $
          traceM $
            "file: " ++ ppShow org
        tell [LintMessage kind code]

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

lintOrgEntry :: Bool -> LintMessageKind -> Entry -> Writer [LintMessage] ()
lintOrgEntry lastEntry level e = do
  checkFor LintError (MisplacedProperty e) $
    any
      ((":properties:" `T.isInfixOf`) . T.toLower)
      (bodyText (has _Paragraph))
  checkFor LintError (MisplacedTimestamp e) $
    any
      ( \t ->
          "SCHEDULED:" `T.isInfixOf` t
            || "DEADLINE:" `T.isInfixOf` t
            || "CLOSED:" `T.isInfixOf` t
      )
      (bodyText (has _Paragraph))
  checkFor LintError (MisplacedLogEntry e) $
    any
      ( \t ->
          "- State " `T.isInfixOf` t
            || "- Note taken " `T.isInfixOf` t
            || ":logbook:" `T.isInfixOf` T.toLower t
      )
      (bodyText (has _Paragraph))
  checkFor LintError (MisplacedDrawerEnd e) $
    any
      ( ( \t ->
            ":end:" `T.isInfixOf` t
              || "#+end" `T.isInfixOf` t
        )
          . T.toLower
      )
      (bodyText (has _Paragraph))
  checkFor LintInfo (TitleWithExcessiveWhitespace e) $
    "  " `T.isInfixOf` (e ^. entryTitle)
  checkFor LintError (DuplicatedProperties e) $
    sort (e ^.. entryProperties . traverse . name)
      /= nub (sort (e ^.. entryProperties . traverse . name))
  checkFor LintWarn (TimestampsOnNonTodo e) $
    not (null (e ^. entryStamps))
      && maybe True (not . isTodo) (e ^? keyword)
  unless lastEntry $
    checkFor LintInfo (UnevenBodyWhitespace e) $
      case e ^. entryText of
        Body [Whitespace _] -> False
        _ -> e ^? entryText . leadSpace /= e ^? entryText . endSpace
  -- jww (2024-05-14): Need to check log entries also
  checkFor LintInfo (EmptyBodyWhitespace e) $
    case e ^. entryText of
      Body [Whitespace _] -> maybe False isTodo (e ^? keyword)
      _ -> False
  checkFor
    LintInfo
    (MultipleBlankLines e)
    (any ((> 1) . length . T.lines) (bodyText (has _Whitespace)))
  checkFor
    LintError
    (MultipleLogbooks e)
    (length (e ^.. entryLogEntries . traverse . cosmos . _LogBook) > 1)
  checkFor
    LintError
    (MixedLogbooks e)
    ( not
        ( null
            ( e
                ^.. entryLogEntries
                  . traverse
                  . _LogBook
                  . traverse
                  . filtered (hasn't _LogClock)
            )
        )
        && not
          ( null
              ( e
                  ^.. entryLogEntries
                    . traverse
                    . filtered (hasn't _LogBook)
              )
          )
    )
  where
    bodyText f =
      e
        ^. entryText
          . blocks
          . traverse
          . filtered f
          . to (showBlock "")
        ++ e
          ^. entryLogEntries
            . traverse
            . failing (_LogState . _4) (_LogNote . _2)
            . _Just
            . blocks
            . traverse
            . filtered f
            . to (showBlock "")

    checkFor ::
      LintMessageKind ->
      LintMessageCode ->
      Bool ->
      Writer [LintMessage] ()
    checkFor kind code b =
      when (b && kind >= level) $ do
        when (level == LintDebug) $
          traceM $
            "entry: " ++ ppShow e
        tell [LintMessage kind code]

showLintOrg :: LintMessage -> String
showLintOrg (LintMessage kind code) =
  renderCode
  where
    entryLoc e =
      e ^. entryFile
        ++ ":"
        ++ show (e ^. entryLine)
        ++ ":"
        ++ show (e ^. entryColumn)
    renderKind = case kind of
      LintError -> "ERROR"
      LintWarn -> "WARN"
      LintInfo -> "INFO"
      LintDebug -> "DEBUG"
    prefix e =
      entryLoc e
        ++ ": "
        ++ renderKind
        ++ " "
    renderCode = case code of
      MisplacedProperty e ->
        prefix e ++ "Misplaced :PROPERTIES: block"
      MisplacedTimestamp e ->
        prefix e ++ "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry e ->
        prefix e ++ "Misplaced state change, note or LOGBOOK"
      MisplacedDrawerEnd e ->
        prefix e ++ "Misplaced end of drawer"
      TitleWithExcessiveWhitespace e ->
        prefix e ++ "Title with excessive whitespace"
      DuplicatedFileProperties f ->
        f ^. filePath ++ ":1: " ++ "Duplicated file properties"
      DuplicatedProperties e ->
        prefix e ++ "Duplicated properties"
      DuplicatedIdentifier ident (e :| es) ->
        prefix e
          ++ "Duplicated identifier "
          ++ T.unpack ident
          ++ "\n"
          ++ intercalate "\n" (map (("  " ++) . entryLoc) es)
      MultipleLogbooks e ->
        prefix e ++ "Multiple logbooks found"
      MixedLogbooks e ->
        prefix e ++ "Log entries inside and outside of logbooks found"
      TimestampsOnNonTodo e ->
        prefix e ++ "Timestamps found on non-todo entry"
      UnevenBodyWhitespace e ->
        prefix e ++ "Whitespace surrounding body is not even"
      UnevenFilePreambleWhitespace f ->
        f ^. filePath
          ++ ":1: "
          ++ "Whitespace surrounding file preamble is not even"
      EmptyBodyWhitespace e ->
        prefix e ++ "Whitespace only body"
      MultipleBlankLines e ->
        prefix e ++ "Multiple blank lines"
