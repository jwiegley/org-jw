{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Lint where

import Control.Applicative
import Control.Lens
import Control.Monad (foldM, unless, when)
import Control.Monad.Writer
import Data.Char (toLower)
import Data.Data.Lens
import Data.Foldable (forM_)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Debug.Trace (traceM)
import Org.Data
import Org.Print
import Org.Types
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Text.Show.Pretty

data LintMessageKind = LintDebug | LintInfo | LintWarn | LintError
  deriving (Show, Eq, Ord)

parseLintMessageKind :: String -> Maybe LintMessageKind
parseLintMessageKind = \case
  "ERROR" -> Just LintError
  "WARN" -> Just LintWarn
  "INFO" -> Just LintInfo
  "DEBUG" -> Just LintDebug
  _ -> Nothing

data TransitionKind
  = FirstTransition
  | IntermediateTransition
  | LastTransition
  deriving (Show, Eq)

data LintMessageCode
  = TodoMissingProperty String
  | FileMissingProperty String
  | TodoLinkDoesNotMatchUrl
  | TodoFileDoesNotMatchAttachment
  | ArchiveTagFileDoesNotExist FilePath
  | TodoLinkKeywordImpliesLinkTag
  | FileSlugMismatch String
  | MisplacedProperty
  | MisplacedTimestamp
  | MisplacedLogEntry
  | MisplacedDrawerEnd
  | DuplicateFileProperty String
  | DuplicateProperty String
  | DuplicateTag String
  | DuplicatedIdentifier String
  | InvalidStateChangeTransitionNotAllowed String (Maybe String) [String]
  | InvalidStateChangeInvalidTransition TransitionKind String String
  | InvalidStateChangeWrongTimeOrder Time Time
  | InvalidStateChangeIdempotent String
  | MultipleLogbooks
  | MixedLogbooks
  | WhitespaceAtStartOfLogEntry
  | TitleWithExcessiveWhitespace
  | TimestampsOnNonTodo
  | UnevenWhitespace String
  | UnevenFilePreambleWhitespace
  | UnnecessaryWhitespace
  | EmptyBodyWhitespace
  | MultipleBlankLines
  | CategoryTooLong String
  | FileCreatedTimeMismatch Time Time
  | TitlePropertyNotLast
  | InvalidLocation String
  deriving (Show, Eq)

data LintMessage = LintMessage
  { lintMsgPos :: Int,
    lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq)

lintOrgFile :: Config -> LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile cfg level org = do
  when (level == LintDebug) $ do
    traceM $ "Linting " ++ (org ^. orgFilePath)
  -- RULE: All files must have ID and CREATED properties
  ruleFileShouldHaveIdAndCreated
  -- RULE: File slugs should reflect the file's title
  ruleSlugMustMatchTitle
  -- RULE: Filenames with dates should have matching CREATED
  ruleCreationTimeMatchesCreated
  -- RULE: Title file property is always last. This is needed for the sake of
  --       xeft and how it displays entry text.
  ruleTitleProperyAlwaysLast
  -- RULE: :ARCHIVE: or #+archive: property alwayos refers to existing file
  ruleArchiveTagFileExists
  forM_ (findDuplicates (props ^.. traverse . name . to (map toLower))) $ \nm ->
    unless (nm `elem` ["link", "tags"]) $
      report LintError (DuplicateFileProperty nm)
  -- checkFor LintInfo (UnevenFilePreambleWhitespace org) $
  --   org ^? fileHeader . headerPreamble . leadSpace
  --     /= org ^? fileHeader . headerPreamble . endSpace
  case reverse (org ^.. allEntries) of
    [] -> pure ()
    e : es -> do
      mapM_
        (lintOrgEntry cfg org False ignoreWhitespace level)
        (reverse es)
      lintOrgEntry cfg org True ignoreWhitespace level e
  where
    ignoreWhitespace = org ^? orgFileProperty "WHITESPACE" == Just "ignore"

    ruleFileShouldHaveIdAndCreated = do
      when (isNothing (org ^? orgFileProperty "ID")) $
        report LintInfo (FileMissingProperty "ID")
      when (isNothing (org ^? orgFileProperty "CREATED")) $
        report
          ( if isNothing (OrgItem org ^? fileCreatedTime)
              then LintWarn
              else LintInfo
          )
          (FileMissingProperty "CREATED")

    ruleSlugMustMatchTitle =
      unless (isJust (org ^? orgFileProperty "NOSLUG")) $
        forM_ (OrgItem org ^? fileSlug) $ \slug ->
          unless (OrgItem org ^? fileActualSlug == OrgItem org ^? fileSlug) $
            report LintInfo (FileSlugMismatch slug)

    ruleCreationTimeMatchesCreated = do
      forM_
        ( (,)
            <$> OrgItem org ^? fileTimestamp
            <*> OrgItem org ^? fileCreatedTime
        )
        $ \(created, created') ->
          unless (created == created') $
            report LintWarn (FileCreatedTimeMismatch created created')

    ruleTitleProperyAlwaysLast =
      forM_
        ( org
            ^? orgFileHeader
              . headerFileProperties
              . _last
              . name
              . to (map toLower)
        )
        $ \lastProp ->
          unless (lastProp == "title") $
            report LintWarn TitlePropertyNotLast

    ruleArchiveTagFileExists = do
      forM_ (org ^? orgFileProperty "ARCHIVE") $ \path -> do
        let path' = takeWhile (/= ':') path
        unless
          ( unsafePerformIO
              (doesFileExist (takeDirectory (org ^. orgFilePath) </> path'))
          )
          $ report LintWarn (ArchiveTagFileDoesNotExist path')

    props =
      org ^. orgFileHeader . headerPropertiesDrawer
        ++ org ^. orgFileHeader . headerFileProperties

    report kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "file: " ++ ppShow org
          tell [LintMessage 1 kind code]
      | otherwise = pure ()

lintOrgEntry ::
  Config ->
  OrgFile ->
  Bool ->
  Bool ->
  LintMessageKind ->
  Entry ->
  Writer [LintMessage] ()
lintOrgEntry cfg org lastEntry ignoreWhitespace level e = do
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
  -- RULE: A :LINK: tag implies a URL property, and vice versa
  ruleLinkTagMatchesUrlProperty
  -- RULE: A LINK keyword implies a :LINK: tag
  ruleLinkKeywordImpliesLinkTag
  -- RULE: An :ARCHIVE: property always refers to an existing file
  ruleArchiveTagFileExists
  -- RULE: A :FILE: tag implies an attachment, and vice versa
  ruleFileTagMatchesAttachment
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
  -- RULE: If there is a LOCATION, it is a valid one
  ruleLocationIsValid
  where
    inArchive = isArchive org

    ruleTodoMustHaveIdAndCreated = do
      let mkw = e ^? entryKeyword . _Just . keywordString
      when (isJust mkw || isJust (e ^? entryCategory)) $ do
        when (isNothing (e ^? property "ID")) $
          report LintWarn (TodoMissingProperty "ID")
        when (isNothing (e ^? property "CREATED")) $
          report LintWarn (TodoMissingProperty "CREATED")

    ruleLinkTagMatchesUrlProperty =
      if e ^? entryKeyword . _Just . keywordString == Just "LINK"
        then
          unless
            ("URL" `elem` e ^.. entryProperties . traverse . name)
            $ report LintWarn TodoLinkDoesNotMatchUrl
        else
          when
            ( (PlainTag "LINK" `elem` e ^. entryTags)
                /= ("URL" `elem` e ^.. entryProperties . traverse . name)
            )
            $ report LintWarn TodoLinkDoesNotMatchUrl

    ruleLinkKeywordImpliesLinkTag = do
      when
        ( (e ^? entryKeyword . _Just . keywordString == Just "LINK")
            && (PlainTag "LINK" `elem` e ^. entryTags)
        )
        $ report LintWarn TodoLinkKeywordImpliesLinkTag

    ruleArchiveTagFileExists = do
      forM_ (e ^? property "ARCHIVE") $ \path -> do
        let path' = takeWhile (/= ':') path
        unless
          ( unsafePerformIO
              (doesFileExist (takeDirectory (org ^. orgFilePath) </> path'))
          )
          $ report LintWarn (ArchiveTagFileDoesNotExist path')

    ruleFileTagMatchesAttachment = do
      when
        ( ("Attachments" `elem` e ^.. entryProperties . traverse . name)
            && (PlainTag "FILE" `notElem` e ^. entryTags)
        )
        $ report LintWarn TodoFileDoesNotMatchAttachment
      forM_ (e ^? property "ID") $ \ident ->
        when
          ( ( (PlainTag "FILE" `elem` e ^. entryTags)
                || ( "Attachments"
                       `elem` e ^.. entryProperties . traverse . name
                   )
            )
              && let dir =
                       (cfg ^. attachmentsDir)
                         </> take 2 ident
                         </> drop 2 ident
                  in not (unsafePerformIO (doesDirectoryExist dir))
          )
          $ report LintWarn TodoFileDoesNotMatchAttachment

    ruleCategoryNameCannotBeTooLong =
      forM_ (e ^? entryCategory) $ \cat ->
        when (length cat > 10) $
          report LintWarn (CategoryTooLong cat)

    paragraphs = bodyString (has _Paragraph)

    rulePropertiesDrawerNeverInBody =
      when
        ( any
            (=~ ("(:properties:|:PROPERTIES:)" :: String))
            paragraphs
        )
        $ report LintError MisplacedProperty

    ruleTimestampsNeverInBody =
      when
        ( any
            (=~ ("(SCHEDULED:|DEADLINE:|CLOSED:)" :: String))
            paragraphs
        )
        $ report LintError MisplacedTimestamp

    ruleLogEntriesNeverInBody =
      when
        ( any
            (=~ ("(- (CLOSING NOTE|State \"|Note taken on|Rescheduled from|Not scheduled, was|New deadline from|Removed deadline, was|Refiled on) |:LOGBOOK:|:logbook:)" :: String))
            paragraphs
        )
        $ report LintError MisplacedLogEntry

    ruleMisplacedDrawerEnd =
      when
        ( any
            (=~ ("(:end:|:END:|#\\+end|#\\+END)" :: String))
            paragraphs
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
      when ("  " `isInfixOf` (e ^. entryTitle)) $
        report LintWarn TitleWithExcessiveWhitespace

    ruleNoDuplicateTags =
      forM_ (findDuplicates (e ^. entryTags)) $ \tag ->
        report LintError (DuplicateTag (tag ^. tagString))

    ruleNoDuplicateProperties =
      forM_
        ( findDuplicates
            ( e
                ^.. entryProperties
                  . traverse
                  . name
                  . to (map toLower)
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
          $ \(mprev, mprevTm) l -> do
            let mkwt' = l ^? _LogState . _2
                mkwf' = l ^? _LogState . _3 . _Just
                mtm = l ^? _LogTime
            forM_ ((,) <$> mtm <*> mprevTm) $ \(tm, prevTm) ->
              when (tm < prevTm) $
                report LintWarn (InvalidStateChangeWrongTimeOrder tm prevTm)
            let mkwt = fmap (^. keywordString) mkwt'
                mkwf = fmap (^. keywordString) mkwf'
                mallowed = transitionsOf cfg <$> mkwf
            unless inArchive $
              forM_ mkwf $ \kwf ->
                case mprev of
                  Nothing ->
                    unless (kwf `elem` ["TODO", "PROJECT"]) $
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
            forM_ mkwt $ \kwt -> do
              if mkwf == Just kwt
                then report LintWarn (InvalidStateChangeIdempotent kwt)
                else forM_ mallowed $ \allowed ->
                  unless (kwt `elem` allowed) $
                    report
                      LintWarn
                      (InvalidStateChangeTransitionNotAllowed kwt mkwf allowed)
            pure (mkwt <|> mprev, mtm <|> mprevTm)
      unless (inArchive || isJust (e ^? property "LAST_REPEAT")) $ do
        let mkw = e ^? entryKeyword . _Just . keywordString
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
      case e
        ^? entryLogEntries
          . traverse
          . cosmos
          . _LogBody
          . blocks
          . _last of
        Just (Whitespace _ txt) -> do
          let ents = e ^.. entryLogEntries . traverse . biplate
          forM_ ents $ \l ->
            forM_ (l ^? _LogBody) $ \logBody -> do
              let good =
                    if last ents == l
                      then case e ^. entryBody of
                        Body [] -> True
                        Body (Whitespace _ _ : _) -> True
                        _ -> logBody ^? endSpace == Just txt
                      else logBody ^? endSpace == Just txt
              unless good $
                report'
                  (l ^. _LogLoc)
                  LintWarn
                  (UnevenWhitespace "log entry")
        _ -> pure ()
      when
        ( not lastEntry && case e ^. entryBody of
            Body [Whitespace _ _] -> False
            b -> b ^? leadSpace /= b ^? endSpace
        )
        $ report LintInfo (UnevenWhitespace "body")

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
    --     ( case e ^. entryBody of
    --         Body [Whitespace _ _] ->
    --           maybe False isTodo (e ^? keyword)
    --         _ -> False
    --     )
    --     $ report LintInfo EmptyBodyWhitespace

    ruleNoUnnecessaryWhitespace = do
      forM_ (e ^.. entryLogEntries . traverse . uniplate) $ \b ->
        when
          ( case b ^? _LogBody of
              Just (Body (Paragraph _ ((' ' : _) : _) : _)) ->
                True
              _ -> False
          )
          $ report' (b ^. _LogLoc) LintInfo UnnecessaryWhitespace
      when
        ( case e ^. entryBody of
            Body (Paragraph _ ((' ' : _) : _) : _) -> True
            _ -> False
        )
        $ report LintInfo UnnecessaryWhitespace

    ruleNoMultipleBlankLines =
      when (any ((> 1) . length . lines) (bodyString (has _Whitespace))) $
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

    ruleLocationIsValid =
      forM_ (e ^? property "LOCATION") $ \loc ->
        when (loc == "0.0,0.0") $
          report LintError (InvalidLocation loc)

    bodyString f =
      e
        ^. entryBody
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
                (loc ^. pos)
                kind
                code
            ]
      | otherwise = pure ()

    report = report' (e ^. entryLoc)

showLintOrg :: FilePath -> LintMessage -> String
showLintOrg fl (LintMessage ln kind code) =
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
          ++ show (fl & fileName . fileNameParts . _2 .~ slug)
      TodoMissingProperty nm ->
        "Open todo missing property " ++ show nm
      FileMissingProperty nm ->
        "File missing property " ++ show nm
      TodoLinkDoesNotMatchUrl ->
        ":LINK: tag does not match URL property"
      TodoFileDoesNotMatchAttachment ->
        ":FILE: tag does not match presence of attachments"
      TodoLinkKeywordImpliesLinkTag ->
        "LINK keyword implies :LINK: tag, therefore it is not needed"
      ArchiveTagFileDoesNotExist path ->
        ":ARCHIVE: tag refers to a non-existent file: " ++ path
      MisplacedProperty ->
        "Misplaced :PROPERTIES: block"
      MisplacedTimestamp ->
        "Misplaced timestamp (SCHEDULED, DEADLINE or CLOSED)"
      MisplacedLogEntry ->
        "Misplaced log entry or log book"
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
      DuplicatedIdentifier ident ->
        "Duplicated identifier " ++ ident
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
        "Wrong time order in log "
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
      UnevenWhitespace desc ->
        "Whitespace surrounding " ++ desc ++ " is not even"
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
      InvalidLocation l ->
        "Location is not valid: " ++ l
