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
import Data.Data.Lens
import Data.Foldable (forM_)
import Data.Hashable
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
-- import System.Directory (getModificationTime)
-- import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import Text.Show.Pretty

-- (=~) ::
--   ( TDFA.RegexMaker
--       TDFA.Regex
--       TDFA.CompOption
--       TDFA.ExecOption
--       a
--   ) =>
--   Text ->
--   a ->
--   Bool
-- (=~) = (TDFA.=~)

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
  = TodoMissingProperty Text
  | FileMissingProperty Text
  | FileSlugMismatch Text
  | MisplacedProperty
  | MisplacedTimestamp
  | MisplacedLogEntry
  | MisplacedDrawerEnd
  | DuplicateFileProperty Text
  | DuplicateProperty Text
  | DuplicateTag Text
  | DuplicatedIdentifier Text (NonEmpty Entry)
  | InvalidStateChangeTransitionNotAllowed Text (Maybe Text) [Text]
  | InvalidStateChangeInvalidTransition TransitionKind Text Text
  | InvalidStateChangeWrongTimeOrder Time Time
  | InvalidStateChangeIdempotent Text
  | MultipleLogbooks
  | MixedLogbooks
  | WhitespaceAtStartOfLogEntry
  | TitleWithExcessiveWhitespace
  | TimestampsOnNonTodo
  | UnevenWhitespace
  | UnevenFilePreambleWhitespace
  | UnnecessaryWhitespace
  | EmptyBodyWhitespace
  | MultipleBlankLines
  | CategoryTooLong Text
  | FileCreatedTimeMismatch Time Time
  | TitlePropertyNotLast
  deriving (Show, Eq, Generic, Data, Typeable, Hashable)

data LintMessage = LintMessage
  { lintMsgFile :: FilePath,
    lintMsgLine :: Int,
    lintMsgKind :: LintMessageKind,
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

  forM_ (M.assocs ids) $ \(k, e :| es) ->
    unless (null es) $
      tell
        [ LintMessage
            (e ^. entryLoc . file)
            (e ^. entryLoc . line)
            LintError
            (DuplicatedIdentifier k (e :| es))
        ]

  let level' =
        fromMaybe
          LintInfo
          (parseMaybe parseLintMessageKind (T.pack level))

  mapM_ (lintOrgFile cfg level') (org ^. orgFiles)

lintOrgFile :: Config -> LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile cfg level org = do
  -- RULE: All files must have ID and CREATED properties
  ruleFileShouldHaveIdAndCreated
  -- RULE: File slugs should reflect the file's title
  ruleSlugMustMatchTitle
  -- RULE: Filenames with dates should have matching CREATED
  ruleCreationTimeMatchesCreated
  -- RULE: Title file property is always last. This is needed for the sake of
  --       xeft and how it displays entry text.
  ruleTitleProperyAlwaysLast
  forM_ (findDuplicates (props ^.. traverse . name . to T.toLower)) $ \nm ->
    unless (nm `elem` ["link", "tags"]) $
      report LintError (DuplicateFileProperty nm)
  -- checkFor LintInfo (UnevenFilePreambleWhitespace org) $
  --   org ^? fileHeader . headerPreamble . leadSpace
  --     /= org ^? fileHeader . headerPreamble . endSpace
  case reverse (org ^.. allEntries) of
    [] -> pure ()
    e : es -> do
      mapM_
        (lintOrgEntry cfg inArchive False ignoreWhitespace level)
        (reverse es)
      lintOrgEntry cfg inArchive True ignoreWhitespace level e
  where
    ignoreWhitespace = org ^? fileProperty "WHITESPACE" == Just "ignore"

    ruleFileShouldHaveIdAndCreated = do
      when (isNothing (org ^? fileProperty "ID")) $
        report LintInfo (FileMissingProperty "ID")
      when (isNothing (org ^? fileProperty "CREATED")) $
        report
          ( if isNothing (org ^? fileCreatedTime)
              then LintWarn
              else LintInfo
          )
          (FileMissingProperty "CREATED")

    ruleSlugMustMatchTitle =
      forM_ (org ^? fileSlug) $ \slug ->
        unless (org ^? fileActualSlug == org ^? fileSlug) $
          report LintInfo (FileSlugMismatch slug)

    ruleCreationTimeMatchesCreated = do
      forM_
        ( (,)
            <$> org ^? fileTimestamp
            <*> org ^? fileCreatedTime
        )
        $ \(created, created') ->
          unless (created == created') $
            report LintWarn (FileCreatedTimeMismatch created created')

    ruleTitleProperyAlwaysLast =
      forM_
        ( org
            ^? fileHeader
              . headerFileProperties
              . _last
              . name
              . to T.toLower
        )
        $ \lastProp ->
          unless (lastProp == "title") $
            report LintWarn TitlePropertyNotLast

    inArchive = isArchive org
    props =
      org ^. fileHeader . headerPropertiesDrawer
        ++ org ^. fileHeader . headerFileProperties

    report kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "file: " ++ ppShow org
          tell [LintMessage (org ^. filePath) 1 kind code]
      | otherwise = pure ()

timestampRe :: Regex
timestampRe =
  case compile
    defaultCompOpt
    defaultExecOpt
    "(SCHEDULED|DEADLINE|CLOSED):" of
    Left err -> error err
    Right x -> x
{-# NOINLINE timestampRe #-}

lintOrgEntry ::
  Config ->
  Bool ->
  Bool ->
  Bool ->
  LintMessageKind ->
  Entry ->
  Writer [LintMessage] ()
lintOrgEntry cfg inArchive lastEntry ignoreWhitespace level e = do
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
  unless ignoreWhitespace ruleNoUnevenWhitespace
  -- RULE: Body and log entry text should never contain only whitespace
  -- ruleNoEmptyBodyWhitespace
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
          report LintWarn (TodoMissingProperty "ID")
        when (isNothing (e ^? createdTime)) $
          report LintWarn (TodoMissingProperty "CREATED")

    ruleCategoryNameCannotBeTooLong =
      forM_ (e ^? entryCategory) $ \cat ->
        when (T.length cat > 10) $
          report LintWarn (CategoryTooLong cat)

    rulePropertiesDrawerNeverInBody =
      when
        ( any
            ((":properties:" `T.isInfixOf`) . T.toLower)
            (bodyText (has _Paragraph))
        )
        $ report LintError MisplacedProperty

    ruleTimestampsNeverInBody =
      when (any (matchTest timestampRe) (bodyText (has _Paragraph))) $
        report LintError MisplacedTimestamp

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
        $ report LintError MisplacedLogEntry

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
        $ report LintError MisplacedDrawerEnd

    ruleNoWhitespaceAtStartOfLogEntry =
      forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
        when
          ( case b ^? _LogBody of
              Just (Body (Whitespace _ _ : _)) -> True
              _ -> False
          )
          $ report' (b ^. _LogLoc) LintWarn WhitespaceAtStartOfLogEntry

    ruleNoExtraSpacesInTitle =
      when ("  " `T.isInfixOf` (e ^. entryTitle)) $
        report LintWarn TitleWithExcessiveWhitespace

    ruleNoDuplicateTags =
      forM_ (findDuplicates (e ^. entryTags)) $ \tag ->
        report LintError (DuplicateTag (tag ^. tagText))

    ruleNoDuplicateProperties =
      forM_
        ( findDuplicates
            ( e
                ^.. entryProperties
                  . traverse
                  . name
                  . to T.toLower
            )
        )
        $ \nm ->
          report LintError (DuplicateProperty nm)

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
                report LintWarn (InvalidStateChangeWrongTimeOrder tm prevTm)
            let kwt = kw' ^. keywordText
                mkwf = fmap (^. keywordText) mkw'
                mallowed = transitionsOf cfg <$> mkwf
            unless inArchive $
              forM_ mkwf $ \kwf ->
                case mprev of
                  Nothing ->
                    unless (kwf `elem` ["TODO", "APPT", "PROJECT"]) $
                      report
                        LintWarn
                        ( InvalidStateChangeInvalidTransition
                            FirstTransition
                            kwf
                            "TODO"
                        )
                  Just prev ->
                    unless
                      ( prev == kwf
                          || isJust (e ^? property "LAST_REPEAT")
                      )
                      $ report
                        LintWarn
                        ( InvalidStateChangeInvalidTransition
                            IntermediateTransition
                            kwf
                            prev
                        )
            if mkwf == Just kwt
              then report LintWarn (InvalidStateChangeIdempotent kwt)
              else forM_ mallowed $ \allowed ->
                unless (kwt `elem` allowed) $
                  report
                    LintWarn
                    (InvalidStateChangeTransitionNotAllowed kwt mkwf allowed)
            pure (Just kwt, Just tm)
      unless (inArchive || isJust (e ^? property "LAST_REPEAT")) $ do
        let mkw = e ^? entryKeyword . _Just . keywordText
        forM_ ((,) <$> mkw <*> mfinalKeyword) $ \(kw, finalKeyword) ->
          unless (kw == finalKeyword) $
            report
              LintWarn
              ( InvalidStateChangeInvalidTransition
                  LastTransition
                  kw
                  finalKeyword
              )
    ruleNoTimestampsOnNonTodos =
      when
        ( any isLeadingStamp (e ^. entryStamps)
            && maybe True (not . isTodo) (e ^? keyword)
        )
        $ report LintWarn TimestampsOnNonTodo

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
            Body [Whitespace _ _] -> False
            b -> b ^? leadSpace /= b ^? endSpace
        )
        $ report LintInfo UnevenWhitespace

    -- ruleNoEmptyBodyWhitespace = do
    --   forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
    --     when
    --       ( case b ^? _LogBody of
    --           Just (Body [Whitespace _ _]) ->
    --             maybe False isTodo (e ^? keyword)
    --           _ -> False
    --       )
    --       $ report' (b ^. _LogLoc) LintInfo EmptyBodyWhitespace
    --   when
    --     ( case e ^. entryText of
    --         Body [Whitespace _ _] ->
    --           maybe False isTodo (e ^? keyword)
    --         _ -> False
    --     )
    --     $ report LintInfo EmptyBodyWhitespace

    ruleNoUnnecessaryWhitespace = do
      forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
        when
          ( case b ^? _LogBody of
              Just (Body (Paragraph _ ((T.unpack -> (' ' : _)) : _) : _)) ->
                True
              _ -> False
          )
          $ report' (b ^. _LogLoc) LintInfo UnnecessaryWhitespace
      when
        ( case e ^. entryText of
            Body (Paragraph _ ((T.unpack -> (' ' : _)) : _) : _) -> True
            _ -> False
        )
        $ report LintInfo UnnecessaryWhitespace

    ruleNoMultipleBlankLines =
      when (any ((> 1) . length . T.lines) (bodyText (has _Whitespace))) $
        report LintWarn MultipleBlankLines

    ruleAtMostOneLogBook =
      when (length (e ^.. entryLogEntries . traverse . cosmos . _LogBook) > 1) $
        report LintError MultipleLogbooks

    ruleConsistentLogBook =
      when
        ( not
            ( null
                ( e
                    ^.. entryLogEntries
                      . traverse
                      . _LogBook
                      . _2
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
        $ report LintError MixedLogbooks

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
            . failing (_LogState . _5) (_LogNote . _3)
            . _Just
            . blocks
            . traverse
            . filtered f
            . to (showBlock "")

    report' loc kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "entry: " ++ ppShow e
          tell
            [ LintMessage
                (loc ^. file)
                (loc ^. line)
                kind
                code
            ]
      | otherwise = pure ()

    report = report' (e ^. entryLoc)

showLintOrg :: LintMessage -> String
showLintOrg (LintMessage fl ln kind code) =
  prefix ++ " " ++ renderCode
  where
    loc = fl ++ ":" ++ show ln
    prefix = loc ++ ": " ++ renderKind
    renderKind = case kind of
      LintError -> "ERROR"
      LintWarn -> "WARN"
      LintInfo -> "INFO"
      LintDebug -> "DEBUG"
    renderCode = case code of
      FileSlugMismatch slug ->
        "Mismatch in file slug:\ngit mv -k -- "
          ++ show fl
          ++ " "
          ++ show (fl & fileName . fileNameParts . _2 .~ T.unpack slug)
      TodoMissingProperty nm ->
        "Open todo missing property " ++ show nm
      FileMissingProperty nm ->
        "File missing property " ++ show nm
      MisplacedProperty ->
        "Misplaced :PROPERTIES: block"
      MisplacedTimestamp ->
        "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry ->
        "Misplaced state change, note or LOGBOOK"
      MisplacedDrawerEnd ->
        "Misplaced end of drawer"
      WhitespaceAtStartOfLogEntry ->
        "Log entry begins with whitespace"
      TitleWithExcessiveWhitespace ->
        "Title with excessive whitespace"
      DuplicateFileProperty nm ->
        "Duplicated file property " ++ show nm
      DuplicateProperty nm ->
        "Duplicated property " ++ show nm
      DuplicateTag nm ->
        "Duplicated tag " ++ show nm
      DuplicatedIdentifier ident (_e :| _es) ->
        "Duplicated identifier " ++ T.unpack ident
      InvalidStateChangeTransitionNotAllowed kwt mkwf allowed ->
        "Transition not allowed "
          ++ show mkwf
          ++ " -> "
          ++ show kwt
          ++ ", allowed: "
          ++ show allowed
      InvalidStateChangeInvalidTransition trans kwt kwf ->
        "Invalid "
          ++ case trans of
            FirstTransition -> "initial"
            IntermediateTransition -> "intermediate"
            LastTransition -> "final"
          ++ " state transition "
          ++ show kwf
          ++ " -> "
          ++ show kwt
      InvalidStateChangeWrongTimeOrder a b ->
        "Wrong time order in state transition "
          ++ show (showTime b)
          ++ " > "
          ++ show (showTime a)
      InvalidStateChangeIdempotent kw ->
        "Idempotent state transition " ++ show kw
      MultipleLogbooks ->
        "Multiple logbooks found"
      MixedLogbooks ->
        "Log entries inside and outside of logbooks found"
      TimestampsOnNonTodo ->
        "Timestamps found on non-todo entry"
      UnevenWhitespace ->
        "Whitespace surrounding body is not even"
      UnevenFilePreambleWhitespace ->
        "Whitespace surrounding file preamble is not even"
      EmptyBodyWhitespace ->
        "Whitespace only body"
      UnnecessaryWhitespace ->
        "Unnecessary whitespace"
      MultipleBlankLines ->
        "Multiple blank lines"
      CategoryTooLong cat ->
        "Category name is too long: " ++ show cat
      FileCreatedTimeMismatch t1 t2 ->
        "Created time does not match file: "
          ++ show (showTime t1)
          ++ " != "
          ++ show (showTime t2)
      TitlePropertyNotLast ->
        "Title is not the last file property"
