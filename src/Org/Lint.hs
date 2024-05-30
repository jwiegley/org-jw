{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Org.Lint where

import Control.Applicative
import Control.Lens
import Control.Monad (foldM, unless, when)
import Control.Monad.Writer
import Data.Data
import Data.Foldable (forM_)
import Data.Hashable
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
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

data TransitionKind
  = FirstTransition
  | IntermediateTransition
  | LastTransition
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

data LintMessageCode
  = TodoMissingProperty Text Entry
  | MisplacedProperty Entry
  | MisplacedTimestamp Entry
  | MisplacedLogEntry Entry
  | MisplacedDrawerEnd Entry
  | DuplicateFileProperty Text OrgFile
  | DuplicateProperty Text Entry
  | DuplicateTag Text Entry
  | DuplicatedIdentifier Text (NonEmpty Entry)
  | InvalidStateChangeTransitionNotAllowed Text (Maybe Text) [Text] Entry
  | InvalidStateChangeInvalidTransition TransitionKind Text Text Entry
  | InvalidStateChangeWrongTimeOrder Time Time Entry
  | InvalidStateChangeIdempotent Text Entry
  | MultipleLogbooks Entry
  | MixedLogbooks Entry
  | WhitespaceAtStartOfLogEntry Entry
  | TitleWithExcessiveWhitespace Entry
  | TimestampsOnNonTodo Entry
  | UnevenWhitespace Entry
  | UnevenFilePreambleWhitespace OrgFile
  | UnnecessaryWhitespace Entry
  | EmptyBodyWhitespace Entry
  | MultipleBlankLines Entry
  | CategoryTooLong Text Entry
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

data LintMessage = LintMessage
  { lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

lintOrgData :: Config -> String -> OrgData -> [LintMessage]
lintOrgData cfg level org = snd . runWriter $ do
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

  mapM_ (lintOrgFile cfg level') (org ^. orgFiles)

lintOrgFile :: Config -> LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile cfg level org = do
  -- jww (2024-05-28): RULE: filenames with dates should have matching CREATED
  -- property
  forM_ (findDuplicates (props ^.. traverse . name)) $ \nm ->
    report LintError (DuplicateFileProperty nm org)
  -- checkFor LintInfo (UnevenFilePreambleWhitespace org) $
  --   org ^? fileHeader . headerPreamble . leadSpace
  --     /= org ^? fileHeader . headerPreamble . endSpace
  case reverse (org ^.. allEntries) of
    [] -> pure ()
    e : es -> do
      mapM_ (lintOrgEntry cfg inArchive False level) (reverse es)
      lintOrgEntry cfg inArchive True level e
  where
    inArchive = isArchive org
    props =
      org ^. fileHeader . headerPropertiesDrawer
        ++ org ^. fileHeader . headerFileProperties

    report kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "file: " ++ ppShow org
          tell [LintMessage kind code]
      | otherwise = pure ()

lintOrgEntry ::
  Config ->
  Bool ->
  Bool ->
  LintMessageKind ->
  Entry ->
  Writer [LintMessage] ()
lintOrgEntry cfg inArchive lastEntry level e = do
  -- jww (2024-05-28): NYI
  -- RULE: No open keywords in archives
  -- RULE: No CREATED date lies in the future
  -- RULE: No title has special characters without escaping
  -- RULE: Leading and trailing whitespace is consistent within log entries
  -- RULE: There is no whitespace preceding the event log
  -- RULE: There is no whitespace after the PROPERTY block (and/or event
  --       log) when there is no whitespace at the end of the entry
  -- RULE: If an entry has trailing whitespace, it's siblings have the same
  --       whitespace
  -- RULE: Don't use :SCRIPT:, use org-babel
  --
  -- RULE: All TODO entries have ID and CREATED properties
  ruleTodoMustHaveIdAndCreated
  -- RULE: Category name should be no longer than 10 characters
  ruleCategoryNameCannotBeTooLong
  -- RULE: PROPERTIES drawer must be at start of entry
  rulePropertiesDrawerNeverInBody
  -- RULE: SCHEDULED, DEADLINE and other timestamps must be at start
  ruleTimestampsNeverInBody
  -- RULE: Log entries must occur before the entry body
  ruleLogEntriesNeverInBody
  -- RULE: Drawer end marker should always properly end a drawer
  ruleMisplacedDrawerEnd
  -- RULE: Log entries should never begin with a blank line
  ruleNoWhitespaceAtStartOfLogEntry
  -- RULE: No title has internal whitespace other than single spaces
  ruleNoExtraSpacesInTitle
  -- RULE: No tag is duplicated
  ruleNoDuplicateTags
  -- RULE: No property is duplicated
  ruleNoDuplicateProperties
  -- RULE: All state changes are well ordered and flow correctly
  ruleNoInvalidStateChanges
  -- RULE: Only TODO items have SCHEDULED/DEADLINE/CLOSED timestamps
  ruleNoTimestampsOnNonTodos
  -- RULE: Whitespace before and after body and log entries should match
  ruleNoUnevenWhitespace
  -- RULE: Body and log entry text should never contain only whitespace
  ruleNoEmptyBodyWhitespace
  -- RULE: No unnecessary leading or trailing whitespace
  ruleNoUnnecessaryWhitespace
  -- RULE: There should never be multiple blank lines
  ruleNoMultipleBlankLines
  -- RULE: There should be at most one logbook
  ruleAtMostOneLogBook
  -- RULE: If there is a logbook, it should contain all CLOCK entries
  ruleConsistentLogBook
  where
    ruleTodoMustHaveIdAndCreated = do
      let mkw = e ^? entryKeyword . _Just . keywordText
      when (isJust mkw || isJust (e ^? entryCategory)) $ do
        when (isNothing (e ^? entryId)) $
          report LintError (TodoMissingProperty "ID" e)
        when (isNothing (e ^? entryCreated)) $
          report LintError (TodoMissingProperty "CREATED" e)

    ruleCategoryNameCannotBeTooLong =
      forM_ (e ^? entryCategory) $ \cat ->
        when (T.length cat > 10) $
          report LintWarn (CategoryTooLong cat e)

    rulePropertiesDrawerNeverInBody =
      when
        ( any
            ((":properties:" `T.isInfixOf`) . T.toLower)
            (bodyText (has _Paragraph))
        )
        $ report LintError (MisplacedProperty e)
    ruleTimestampsNeverInBody =
      when
        ( any
            ( \t ->
                "SCHEDULED:" `T.isInfixOf` t
                  || "DEADLINE:" `T.isInfixOf` t
                  || "CLOSED:" `T.isInfixOf` t
            )
            (bodyText (has _Paragraph))
        )
        $ report LintError (MisplacedTimestamp e)
    ruleLogEntriesNeverInBody =
      when
        ( any
            ( \t ->
                "- CLOSING NOTE " `T.isInfixOf` t
                  || "- State " `T.isInfixOf` t
                  || "- Note taken on " `T.isInfixOf` t
                  || "- Rescheduled from " `T.isInfixOf` t
                  || "- Not scheduled, was " `T.isInfixOf` t
                  || "- New deadline from " `T.isInfixOf` t
                  || "- Removed deadline, was " `T.isInfixOf` t
                  || "- Refiled on " `T.isInfixOf` t
                  || ":logbook:" `T.isInfixOf` T.toLower t
            )
            (bodyText (has _Paragraph))
        )
        $ report LintError (MisplacedLogEntry e)
    ruleMisplacedDrawerEnd =
      when
        ( any
            ( ( \t ->
                  ":end:" `T.isInfixOf` t
                    || "#+end" `T.isInfixOf` t
              )
                . T.toLower
            )
            (bodyText (has _Paragraph))
        )
        $ report LintError (MisplacedDrawerEnd e)
    ruleNoWhitespaceAtStartOfLogEntry =
      forM_ (e ^.. entryLogEntries . traverse . cosmos . _LogBody) $ \b ->
        when
          ( case b of
              Body (Whitespace _ : _) -> True
              _ -> False
          )
          $ report LintWarn (WhitespaceAtStartOfLogEntry e)
    ruleNoExtraSpacesInTitle =
      when ("  " `T.isInfixOf` (e ^. entryTitle)) $
        report LintInfo (TitleWithExcessiveWhitespace e)
    ruleNoDuplicateTags =
      forM_ (findDuplicates (e ^.. entryTags . traverse . tagText)) $ \nm ->
        report LintError (DuplicateTag nm e)
    ruleNoDuplicateProperties =
      forM_ (findDuplicates (e ^.. entryProperties . traverse . name)) $ \nm ->
        report LintError (DuplicateProperty nm e)
    ruleNoInvalidStateChanges = do
      (mfinalKeyword, _mfinalTime) <-
        ( \f ->
            foldM
              f
              ( Nothing,
                Nothing
              )
              -- jww (2024-05-28): Only reverse here if the configuration
              -- indicates that state entries are from most recent to least
              -- recent.
              (reverse (e ^.. entryStateHistory))
          )
          $ \(mprev, mprevTm) (kw', mkw', tm) -> do
            forM_ mprevTm $ \prevTm ->
              when (tm < prevTm) $
                report LintWarn (InvalidStateChangeWrongTimeOrder tm prevTm e)
            let kwt = kw' ^. keywordText
                mkwf = fmap (^. keywordText) mkw'
                mallowed = transitionsOf cfg <$> mkwf
            unless inArchive $
              forM_ mkwf $ \kwf ->
                case mprev of
                  Nothing ->
                    unless (kwf `elem` ["TODO", "APPT", "PROJECT"]) $
                      report
                        LintInfo
                        ( InvalidStateChangeInvalidTransition
                            FirstTransition
                            kwf
                            "TODO"
                            e
                        )
                  Just prev ->
                    unless
                      ( prev == kwf
                          || isJust (e ^? property "LAST_REPEAT")
                      )
                      $ report
                        LintInfo
                        ( InvalidStateChangeInvalidTransition
                            IntermediateTransition
                            kwf
                            prev
                            e
                        )
            if mkwf == Just kwt
              then report LintWarn (InvalidStateChangeIdempotent kwt e)
              else forM_ mallowed $ \allowed ->
                unless (kwt `elem` allowed) $
                  report
                    LintWarn
                    (InvalidStateChangeTransitionNotAllowed kwt mkwf allowed e)
            pure (Just kwt, Just tm)
      unless (inArchive || isJust (e ^? property "LAST_REPEAT")) $ do
        let mkw = e ^? entryKeyword . _Just . keywordText
        forM_ ((,) <$> mkw <*> mfinalKeyword) $ \(kw, finalKeyword) ->
          unless (kw == finalKeyword) $
            report
              LintInfo
              ( InvalidStateChangeInvalidTransition
                  LastTransition
                  kw
                  finalKeyword
                  e
              )
    ruleNoTimestampsOnNonTodos =
      when
        ( not (null (e ^. entryStamps))
            && maybe True (not . isTodo) (e ^? keyword)
        )
        $ report LintWarn (TimestampsOnNonTodo e)
    ruleNoUnevenWhitespace = do
      -- jww (2024-05-28): If the first log entry ends with a blank line, then
      -- all of them should, except when there is no body text in which case
      -- the last log entry should not end with whitespace.
      -- forM_ (e ^.. entryLogEntries . traverse . cosmos . _LogBody) $ \b ->
      --   when
      --     ( case b of
      --         Body [Whitespace _] -> False
      --         _ -> b ^? leadSpace /= b ^? endSpace
      --     )
      --     $ report LintInfo (UnevenWhitespace e)
      when
        ( not lastEntry && case e ^. entryText of
            Body [Whitespace _] -> False
            b -> b ^? leadSpace /= b ^? endSpace
        )
        $ report LintInfo (UnevenWhitespace e)
    ruleNoEmptyBodyWhitespace = do
      forM_ (e ^.. entryLogEntries . traverse . cosmos . _LogBody) $ \b ->
        when
          ( case b of
              Body [Whitespace _] -> maybe False isTodo (e ^? keyword)
              _ -> False
          )
          $ report LintInfo (EmptyBodyWhitespace e)
      when
        ( case e ^. entryText of
            Body [Whitespace _] -> maybe False isTodo (e ^? keyword)
            _ -> False
        )
        $ report LintInfo (EmptyBodyWhitespace e)
    ruleNoUnnecessaryWhitespace = do
      forM_ (e ^.. entryLogEntries . traverse . cosmos . _LogBody) $ \b ->
        when
          ( case b of
              Body (Paragraph ((T.unpack -> (' ' : _)) : _) : _) -> True
              _ -> False
          )
          $ report LintInfo (UnnecessaryWhitespace e)
      when
        ( case e ^. entryText of
            Body (Paragraph ((T.unpack -> (' ' : _)) : _) : _) -> True
            _ -> False
        )
        $ report LintInfo (UnnecessaryWhitespace e)
    ruleNoMultipleBlankLines =
      when (any ((> 1) . length . T.lines) (bodyText (has _Whitespace))) $
        report LintInfo (MultipleBlankLines e)
    ruleAtMostOneLogBook =
      when (length (e ^.. entryLogEntries . traverse . cosmos . _LogBook) > 1) $
        report LintError (MultipleLogbooks e)
    ruleConsistentLogBook =
      when
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
        $ report LintError (MixedLogbooks e)

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

    report kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "entry: " ++ ppShow e
          tell [LintMessage kind code]
      | otherwise = pure ()

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
      TodoMissingProperty nm e ->
        prefix e ++ "Open todo missing property " ++ show nm
      MisplacedProperty e ->
        prefix e ++ "Misplaced :PROPERTIES: block"
      MisplacedTimestamp e ->
        prefix e ++ "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry e ->
        prefix e ++ "Misplaced state change, note or LOGBOOK"
      MisplacedDrawerEnd e ->
        prefix e ++ "Misplaced end of drawer"
      WhitespaceAtStartOfLogEntry e ->
        prefix e ++ "Log entry begins with whitespace"
      TitleWithExcessiveWhitespace e ->
        prefix e ++ "Title with excessive whitespace"
      DuplicateFileProperty nm f ->
        f ^. filePath ++ ":1: " ++ "Duplicated file property " ++ show nm
      DuplicateProperty nm e ->
        prefix e ++ "Duplicated property " ++ show nm
      DuplicateTag nm e ->
        prefix e ++ "Duplicated tag " ++ show nm
      DuplicatedIdentifier ident (e :| es) ->
        prefix e
          ++ "Duplicated identifier "
          ++ T.unpack ident
          ++ "\n"
          ++ intercalate "\n" (map (("  " ++) . entryLoc) es)
      InvalidStateChangeTransitionNotAllowed kwt mkwf allowed e ->
        prefix e
          ++ "Transition not allowed "
          ++ show mkwf
          ++ " -> "
          ++ show kwt
          ++ ", allowed: "
          ++ show allowed
      InvalidStateChangeInvalidTransition trans kwt kwf e ->
        prefix e
          ++ "Invalid "
          ++ case trans of
            FirstTransition -> "initial"
            IntermediateTransition -> "intermediate"
            LastTransition -> "final"
          ++ " state transition "
          ++ show kwf
          ++ " -> "
          ++ show kwt
      InvalidStateChangeWrongTimeOrder after before e ->
        prefix e
          ++ "Wrong time order in state transition "
          ++ show (showTime before)
          ++ " > "
          ++ show (showTime after)
      InvalidStateChangeIdempotent kw e ->
        prefix e ++ "Idempotent state transition " ++ show kw
      MultipleLogbooks e ->
        prefix e ++ "Multiple logbooks found"
      MixedLogbooks e ->
        prefix e ++ "Log entries inside and outside of logbooks found"
      TimestampsOnNonTodo e ->
        prefix e ++ "Timestamps found on non-todo entry"
      UnevenWhitespace e ->
        prefix e ++ "Whitespace surrounding body is not even"
      UnevenFilePreambleWhitespace f ->
        f ^. filePath
          ++ ":1: "
          ++ "Whitespace surrounding file preamble is not even"
      EmptyBodyWhitespace e ->
        prefix e ++ "Whitespace only body"
      UnnecessaryWhitespace e ->
        prefix e ++ "Unnecessary whitespace"
      MultipleBlankLines e ->
        prefix e ++ "Multiple blank lines"
      CategoryTooLong cat e ->
        prefix e ++ "Category name is too long: " ++ show cat
