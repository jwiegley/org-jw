{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Lint where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad (foldM, unless, when)
import Control.Monad.Reader
import Control.Monad.Writer
import Crypto.Hash.SHA512
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isLower, isUpper, toLower)
import Data.Data (Data)
import Data.Data.Lens
import Data.Foldable (Foldable (..), forM_)
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Typeable (Typeable)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Org.Data
import Org.Print
import Org.Types
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getHomeDirectory,
  )
import System.Exit
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Text.Show.Pretty
import Prelude hiding (Foldable (..))

consistent :: (Eq a) => [a] -> Bool
consistent [] = True
consistent (x : xs) = foldl' (\b y -> b && x == y) True xs

data LintMessageKind = LintDebug | LintAll | LintInfo | LintWarn | LintError
  deriving (Data, Show, Eq, Typeable, Generic, Enum, Bounded, Ord, NFData)

parseLintMessageKind :: String -> Maybe LintMessageKind
parseLintMessageKind = \case
  "ERROR" -> Just LintError
  "WARN" -> Just LintWarn
  "INFO" -> Just LintInfo
  "ALL" -> Just LintAll
  "DEBUG" -> Just LintDebug
  _ -> Nothing

data TransitionKind
  = FirstTransition
  | IntermediateTransition
  | LastTransition
  deriving (Show, Eq, Generic, NFData)

data LintMessageCode
  = TodoMissingProperty String
  | FileMissingProperty String
  | TaskMissingAssignment
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
  | FileTitleMissing
  | TitleWithExcessiveWhitespace
  | OverlyLongHeadline
  | TimestampsOnNonTodo
  | InconsistentWhitespace String
  | InconsistentFilePreambleWhitespace
  | UnnecessaryWhitespace
  | EmptyBodyWhitespace
  | MultipleBlankLines
  | CategoryTooLong String
  | FileCreatedTimeMismatch Time Time
  | TitlePropertyNotLast
  | FileTagsTodoMismatch
  | VerbInFileUnknown String
  | TagInFileUnknown String
  | InvalidLocation String
  | InvalidDrawerCase DrawerType
  | TodoMissingReviewProperties
  | NonTodoWithReviewProperties
  | BrokenLink String
  | HashesDoNotMatch String String
  | FileFailsToRoundTrip
  | AudioFileNotFound FilePath
  deriving (Show, Eq, Generic, NFData)

data LintMessage = LintMessage
  { lintMsgPos :: Int,
    lintMsgKind :: LintMessageKind,
    lintMsgCode :: LintMessageCode
  }
  deriving (Show, Eq, Generic, NFData)

lintOrgFiles ::
  Config ->
  LintMessageKind ->
  [OrgFile] ->
  Map FilePath [LintMessage]
lintOrgFiles cfg level xs =
  let (entriesById, ms) = foldr doLint (M.empty, []) xs
      idMsgs = flip concatMap (M.assocs entriesById) $ \(k, loc :| locs) ->
        [ ( loc ^. file,
            [ LintMessage
                (loc ^. pos)
                LintError
                (DuplicatedIdentifier k)
            ]
          )
          | not (null locs)
        ]
   in M.unionWith (<>) (M.fromList ms) (M.fromList idMsgs)
  where
    doLint ::
      OrgFile ->
      (Map String (NonEmpty Loc), [(FilePath, [LintMessage])]) ->
      (Map String (NonEmpty Loc), [(FilePath, [LintMessage])])
    doLint org (entriesById, ms) =
      (entriesById', (org ^. orgFilePath, msgs) : ms)
      where
        entriesById' =
          (\f -> foldr f entriesById (org ^.. allEntries)) $ \e m ->
            let loc = e ^. entryLoc
             in maybe
                  m
                  ( \ident ->
                      m
                        & at ident
                          %~ Just
                            . maybe
                              (NE.singleton loc)
                              (NE.cons loc)
                  )
                  (e ^? entryId)
        msgs = lintOrgFile cfg level org

lintOrgFile :: Config -> LintMessageKind -> OrgFile -> [LintMessage]
lintOrgFile cfg level org = execWriter (lintOrgFile' cfg level org)

lintOrgFile' :: Config -> LintMessageKind -> OrgFile -> Writer [LintMessage] ()
lintOrgFile' cfg level org = do
  when (level == LintDebug) $ do
    traceM $ "Linting " ++ (org ^. orgFilePath)
  -- RULE: All files must have titles
  ruleFileShouldHaveTitle
  -- RULE: All files must have ID and CREATED properties
  ruleFileShouldHaveIdAndCreated
  -- RULE: File slugs should reflect the file's title
  ruleSlugMustMatchTitle
  -- RULE: Filenames with dates should have matching CREATED
  ruleCreationTimeMatchesCreated
  -- RULE: Title file property is always last. This is needed for the sake of
  --       xeft and how it displays entry text.
  ruleTitleProperyAlwaysLast
  -- rule: :ARCHIVE: or #+archive: property alwayos refers to existing file
  ruleArchiveTagFileExists
  -- RULE: A filetags of :todo: should indicate open TODO entries
  ruleFileTagsTodo
  -- RULE: Files do not have NEXT/LAST_REVIEW properties
  ruleOnlyTodosReview
  -- RULE: All tags are part of the tags vocabulary, if specified
  ruleTagsVocabulary
  -- RULE: All verbs are part of the verb vocabulary, if specified
  ruleVerbVocabulary
  -- RULE: Check that all file links point to actual files
  ruleCheckAllLinks
  -- RULE: Check that AUDIO property points to an existing file
  ruleAudioFileExists
  -- RULE: No duplicated file properties outside of link and tags
  forM_ (findDuplicates (props ^.. traverse . name . to (map toLower))) $ \nm ->
    unless (nm `elem` ["link", "tags"]) $
      report LintError (DuplicateFileProperty nm)
  -- RULE: Whitespace before and after body should match
  -- checkFor LintInfo (InconsistentFilePreambleWhitespace org) $
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

    ruleFileShouldHaveTitle =
      when (isNothing (org ^? orgFileProperty "title")) $
        report LintInfo FileTitleMissing

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

    ruleCreationTimeMatchesCreated =
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

    ruleArchiveTagFileExists =
      forM_ (org ^? orgFileProperty "ARCHIVE") $ \path -> do
        let path' = takeWhile (/= ':') path
        unless
          ( unsafePerformIO
              (doesFileExist (takeDirectory (org ^. orgFilePath) </> path'))
          )
          $ report LintWarn (ArchiveTagFileDoesNotExist path')

    ruleFileTagsTodo =
      unless (isJust (org ^? orgFileProperty "HAS_TODO")) $
        forM_ (OrgItem org ^? fileTags) $ \tags ->
          unless
            ( ( if PlainTag "todo" `elem` tags
                  then id
                  else not
              )
                $ any id (org ^.. allEntries . keyword . to (isOpenTodo cfg))
            )
            $ report LintWarn FileTagsTodoMismatch

    ruleOnlyTodosReview =
      when
        ( isJust (org ^? orgFileProperty "LAST_REVIEW")
            || isJust (org ^? orgFileProperty "NEXT_REVIEW")
            || isJust (org ^? orgFileProperty "REVIEWS")
            || isJust (org ^? orgFileProperty "Effort")
        )
        $ report LintWarn NonTodoWithReviewProperties

    ruleTagsVocabulary =
      forM_ (org ^? orgFileProperty "TAGS_ALL") $ \tags -> do
        let tags' = words tags
        forM_ (org ^.. allEntries) $ \e ->
          forM_ (e ^.. entryTags . traverse) $ \(PlainTag entryTag) ->
            unless (entryTag `elem` tags') $
              report' (e ^. entryLoc . pos) LintWarn $
                TagInFileUnknown entryTag

    ruleVerbVocabulary =
      forM_ (org ^? orgFileProperty "VERB_ALL") $ \verbs -> do
        let verbs' = words verbs
        forM_ (org ^.. allEntries) $ \e ->
          forM_ (e ^.. entryVerb . _Just) $ \verb ->
            unless (verb `elem` verbs') $
              report' (e ^. entryLoc . pos) LintWarn $
                VerbInFileUnknown verb

    ruleCheckAllLinks =
      unless (isJust (org ^? orgFileProperty "IGNORE_LINKS")) $
        forM_ paragraphs $ \paragraph ->
          case paragraph
            =~ ("\\[\\[(file:|https?:)(~/)?([^]:]+)" :: String) of
            AllTextSubmatches ([_, protocol, tilde, link] :: [String]) ->
              unless
                ( if protocol == "file"
                    then pathExists tilde (org ^. orgFilePath) link
                    else level > LintAll || urlExists (protocol ++ link)
                )
                $ report
                  LintError
                  ( BrokenLink
                      ( if protocol == "file"
                          then tilde ++ link
                          else protocol ++ link
                      )
                  )
            _ -> pure ()

    ruleAudioFileExists =
      forM_ (org ^? orgFileProperty "AUDIO") $ \audioPath -> do
        let normalizedPath = case audioPath of
              ('~' : '/' : rest) -> do
                let home = unsafePerformIO getHomeDirectory
                home </> rest
              _ -> takeDirectory (org ^. orgFilePath) </> audioPath
        unless (unsafePerformIO (doesFileExist normalizedPath)) $
          report LintError (AudioFileNotFound audioPath)

    paragraphs = bodyString (has _Paragraph)

    bodyString f =
      org
        ^. orgFileHeader
          . headerPreamble
          . blocks
          . traverse
          . filtered f
          . to (\b -> runReader (showBlock "" b) cfg)

    props =
      org ^. orgFileHeader . headerPropertiesDrawer
        ++ org ^. orgFileHeader . headerFileProperties

    report' loc kind code
      | kind >= level = do
          when (level == LintDebug) $
            traceM $
              "file: " ++ ppShow org
          tell [LintMessage loc kind code]
      | otherwise = pure ()

    report = report' 1

lintOrgEntry ::
  Config ->
  OrgFile ->
  Bool ->
  Bool ->
  LintMessageKind ->
  Entry ->
  Writer [LintMessage] ()
lintOrgEntry cfg org isLastEntry ignoreWhitespace level e = do
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
  -- RULE: All TASK entries must have a tag-indicated assignment
  ruleTaskMustHaveAssignment
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
  -- RULE: Check that all file links point to actual files
  ruleCheckAllLinks
  -- RULE: Log entries should never begin with a blank line
  ruleNoWhitespaceAtStartOfLogEntry
  -- RULE: No title has internal whitespace other than single spaces
  ruleNoExtraSpacesInTitle
  -- RULE: No headline is too long
  -- ruleNoOverlyLongHeadline
  -- RULE: No tag is duplicated
  ruleNoDuplicateTags
  -- RULE: No property is duplicated
  ruleNoDuplicateProperties
  -- RULE: All state changes are well ordered and flow correctly
  ruleNoInvalidStateChanges
  -- RULE: Only TODO items have SCHEDULED/DEADLINE/CLOSED timestamps
  ruleNoTimestampsOnNonTodos
  -- RULE: Only and all TODO items have NEXT/LAST_REVIEW properties
  ruleOnlyTodosReview
  -- RULE: Whitespace before and after body and log entries should match
  unless ignoreWhitespace ruleNoInconsistentWhitespace
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
  -- RULE: If there is a LOCATION, it is a valid one
  ruleLocationIsValid
  -- RULE: Plain drawers are uppercase, begin/end drawers are lowercase
  ruleDrawerCase
  -- RULE: Entries with hashes match when hashed
  ruleHashesMatch
  where
    inArchive = isArchive org

    ruleTodoMustHaveIdAndCreated = do
      let mkw = e ^? entryKeyword . _Just . keywordString
      when (isJust mkw || isJust (e ^? entryCategory)) $ do
        when (isNothing (e ^? property "ID")) $
          report LintWarn (TodoMissingProperty "ID")
        when (isNothing (e ^? property "CREATED")) $
          report LintWarn (TodoMissingProperty "CREATED")

    ruleTaskMustHaveAssignment = do
      let mkw = e ^? entryKeyword . _Just . keywordString
      when (mkw == Just "TASK" && null (e ^. entryTags)) $
        report LintWarn TaskMissingAssignment

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

    ruleCheckAllLinks =
      unless (isJust (org ^? orgFileProperty "IGNORE_LINKS")) $ do
        forM_ (e ^? property "URL") $ \doc ->
          case doc
            =~ ("\\[\\[(file:|https?:)(~/)?([^]:]+)" :: String) of
            AllTextSubmatches ([_, protocol, tilde, link] :: [String]) ->
              unless
                ( if protocol == "file"
                    then pathExists tilde (org ^. orgFilePath) link
                    else level > LintAll || urlExists (protocol ++ link)
                )
                $ report
                  LintError
                  ( BrokenLink
                      ( if protocol == "file"
                          then tilde ++ link
                          else protocol ++ link
                      )
                  )
            _ -> pure ()
        forM_ (e ^? property "NOTER_DOCUMENT") $ \doc ->
          case doc =~ ("(~/)?([^]:]+)" :: String) of
            AllTextSubmatches ([_, tilde, link] :: [String]) ->
              unless
                ( pathExists tilde (org ^. orgFilePath) link
                    || "devonthink" `isInfixOf` link
                )
                $ report LintError (BrokenLink (tilde ++ link))
            _ -> pure ()
        forM_ paragraphs $ \paragraph ->
          case paragraph
            =~ ("\\[\\[(file:|https?:)(~/)?([^]:]+)" :: String) of
            AllTextSubmatches ([_, protocol, tilde, link] :: [String]) ->
              unless
                ( if protocol == "file"
                    then pathExists tilde (org ^. orgFilePath) link
                    else level > LintAll || urlExists (protocol ++ link)
                )
                $ report
                  LintError
                  ( BrokenLink
                      ( if protocol == "file"
                          then tilde ++ link
                          else protocol ++ link
                      )
                  )
            _ -> pure ()

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

    _ruleNoOverlyLongHeadline =
      when (length (e ^. entryHeadline) > (96 - e ^. entryDepth)) $
        report LintWarn OverlyLongHeadline

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
                    unless (kwf `elem` cfg ^. startKeywords) $
                      report
                        LintWarn
                        ( InvalidStateChangeInvalidTransition
                            FirstTransition
                            kwf
                            (cfg ^?! startKeywords . _head)
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
            && maybe True (not . isTodo cfg) (e ^? keyword)
        )
        $ report LintWarn TimestampsOnNonTodo

    ruleOnlyTodosReview =
      when
        ( maybe True (not . isTodo cfg) (e ^? keyword)
            && ( isJust (e ^? property "LAST_REVIEW")
                   || isJust (e ^? property "NEXT_REVIEW")
                   || isJust (e ^? property "REVIEWS")
                   || isJust (e ^? property "Effort")
               )
        )
        $ report LintWarn NonTodoWithReviewProperties

    ruleNoInconsistentWhitespace = do
      unless (consistent logLeading) $
        report LintWarn (InconsistentWhitespace "before log entries")
      unless (consistent logTrailing) $
        report LintWarn (InconsistentWhitespace "after log entries")
      when
        ( isBodyEmpty
            && isJust
              ( e
                  ^? entryLogEntries
                    . _last
                    . _LogBody
                    . blocks
                    . _last
                    . _Whitespace
              )
        )
        $ report LintInfo EmptyBodyWhitespace
      unless
        ( (isLastEntry && bodyTrailing == Nothing)
            || bodyLeading == bodyTrailing
        )
        $ report LintInfo (InconsistentWhitespace "surrounding body")
      where
        bodyLeading = do
          ws <- bodyWhitespace _head
          ws ^? _Whitespace . _2
        bodyTrailing = do
          ws <- bodyWhitespace _last
          ws ^? _Whitespace . _2
        bodyWhitespace f = e ^? entryBody . blocks . f
        isBodyEmpty = null (e ^. entryBody . blocks)
        logLeading = map (^? _Whitespace . _2) (logWhitespace _head)
        logTrailing =
          logTrailing'
            & _last %~ \case
              Nothing
                | isBodyEmpty -> logTrailing' ^? _head . _Just
                | Just ws <- bodyLeading -> do
                    _ <- logTrailing' ^? _head . _Just
                    pure ws
                | otherwise -> Nothing
              x -> x
        logTrailing' = map (^? _Whitespace . _2) (logWhitespace _last)
        logWhitespace f =
          e
            ^.. entryLogEntries
              . traverse
              . cosmos
              . _LogBody
              . blocks
              . f

    ruleNoEmptyBodyWhitespace =
      when
        ( case e ^. entryBody of
            Body [Whitespace _ _] ->
              ( e ^. entryTitle
                  `elem` [ "Attending",
                           "Agenda",
                           "Minutes",
                           "Notes",
                           "Transcript"
                         ]
                  && null (e ^. entryItems)
              )
                || maybe False (isTodo cfg) (e ^? keyword)
            _ -> False
        )
        $ report LintInfo EmptyBodyWhitespace

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

    ruleDrawerCase =
      forM_
        ( e
            ^.. entryBody
              . blocks
              . traverse
              . _Drawer
              . _2
        )
        $ \drawerType ->
          unless
            ( case drawerType of
                PlainDrawer nm ->
                  all (\c -> c == ':' || isUpper c) (words nm ^?! _head)
                BeginDrawer nm ->
                  all
                    ( \c ->
                        c `elem` ['#', '+', '_', ':']
                          || isLower c
                    )
                    (words nm ^?! _head)
            )
            $ report LintInfo (InvalidDrawerCase drawerType)

    ruleHashesMatch =
      forM_ (e ^? property "HASH_sha512") $ \definedHash ->
        let entryWithoutHash =
              e & entryProperties %~ filter (\p -> p ^. name /= "HASH_sha512")
            actualHash = hashEntry entryWithoutHash
         in when (definedHash /= actualHash) $
              report LintWarn (HashesDoNotMatch definedHash actualHash)
      where
        hashEntry ent =
          take 64 $
            T.unpack $
              T.decodeUtf8 $
                Base16.encode $
                  hash $
                    T.encodeUtf8 $
                      T.pack $
                        (++ "\n") $
                          intercalate "\n" $
                            runReader (showEntry ent) cfg

    bodyString f =
      e
        ^. entryBody
          . blocks
          . traverse
          . filtered f
          . to (\b -> runReader (showBlock "" b) cfg)
        ++ e
          ^. entryLogEntries
            . traverse
            . failing (_LogState . _5) (_LogNote . _3)
            . _Just
            . blocks
            . traverse
            . filtered f
            . to (\b -> runReader (showBlock "" b) cfg)

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

pathExists :: String -> FilePath -> FilePath -> Bool
pathExists tilde path link = unsafePerformIO $ do
  home <- getHomeDirectory
  doesPathExist
    ( if tilde == "~/"
        then home </> link
        else takeDirectory path </> link
    )

urlExists :: String -> Bool
urlExists url = unsafePerformIO $ do
  (ec, _, _) <-
    readProcessWithExitCode
      "curl"
      [ "--output",
        "/dev/null",
        "--silent",
        "--head",
        "--fail",
        "--connect-timeout",
        "5",
        url
      ]
      ""
  pure $ ec == ExitSuccess

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
      LintAll -> "ALL"
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
      TaskMissingAssignment ->
        "Task missing assignment"
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
      FileTitleMissing ->
        "Title is missing"
      TitleWithExcessiveWhitespace ->
        "Title with excessive whitespace"
      OverlyLongHeadline ->
        "Headline is too long"
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
      InconsistentWhitespace desc ->
        "Whitespace " ++ desc ++ " is inconsistent"
      InconsistentFilePreambleWhitespace ->
        "Whitespace surrounding file preamble is inconsistent"
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
      FileTagsTodoMismatch ->
        "Filetags :todo: does not reflect todo entries in file"
      TagInFileUnknown tag ->
        "Tag in file is not part of tags vocabulary: " ++ tag
      VerbInFileUnknown tag ->
        "Verb in file is not part of verb vocabulary: " ++ tag
      InvalidLocation l ->
        "Location is not valid: " ++ l
      InvalidDrawerCase d ->
        "Drawer has invalid case: " ++ show d
      TodoMissingReviewProperties ->
        "Todo missing LAST_REVIEW and NEXT_REVIEW properties"
      NonTodoWithReviewProperties ->
        "Non-todo with LAST_REVIEW and NEXT_REVIEW properties"
      BrokenLink link ->
        "Link to missing file: " ++ link
      HashesDoNotMatch x y ->
        "Hashes do not match: " ++ x ++ " != " ++ y
      FileFailsToRoundTrip ->
        "File fails to round trip through parsing and printing"
      AudioFileNotFound path ->
        "Audio file referenced in :AUDIO: property not found: " ++ path
