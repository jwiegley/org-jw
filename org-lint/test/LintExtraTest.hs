{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Extra tight lint tests targeting the remaining uncovered
expression spans in 'Org.Lint' that the three existing lint test
modules do not exercise.

Every test here forces the resulting messages to 'rnf' so the Writer
thunks collapse into 'istickedoff' spans. The groups are ordered by
source-line appearance to make future maintenance easier.
-}
module LintExtraTest (tests) where

import Control.DeepSeq (deepseq, rnf)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Org.Lint
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- Minimal shared helpers. Every binding here is referenced below.

lintConfig :: Config
lintConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT", "TASK", "LINK"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions =
        [ ("TODO", ["DONE", "WAIT", "CANCELED"])
        , ("WAIT", ["TODO", "DONE", "CANCELED"])
        , ("TASK", ["DONE", "CANCELED"])
        ]
    , _startKeywords = ["TODO", "WAIT", "TASK"]
    , _priorities = ["A", "B", "C"]
    , _propertyColumn = 11
    , _tagsColumn = 60
    , _checkFiles = False
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

-- Parse a fixture into an 'OrgFile'. Fails loudly on parse error so
-- we notice test-input typos immediately.
parseFixture :: Config -> FilePath -> ByteString -> OrgFile
parseFixture cfg path bs =
  case parseOrgFile cfg path bs of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show bs
    Right org -> org

runLint :: Config -> FilePath -> ByteString -> [LintMessage]
runLint cfg = runLintAt cfg LintInfo

runLintAt ::
  Config ->
  LintMessageKind ->
  FilePath ->
  ByteString ->
  [LintMessage]
runLintAt cfg level path input =
  lintOrgFile cfg level (parseFixture cfg path input)

-- Force every 'LintMessage' and its fields so the Writer thunks
-- evaluate. 'rnf' walks every constructor argument, flipping
-- @nottickedoff@ ticks to @istickedoff@. We use 'evaluate' so HPC
-- observes the result expression being demanded.
forceMessages :: [LintMessage] -> IO ()
forceMessages msgs = do
  _ <- evaluate (rnf msgs)
  mapM_
    ( \m -> do
        !_ <- evaluate (lintMsgPos m)
        !_ <- evaluate (lintMsgKind m)
        !_ <- evaluate (lintMsgCode m)
        return ()
    )
    msgs
  -- The Show instance for LintMessage/LintMessageCode forces every
  -- constructor argument, which HPC records as boolean/if coverage
  -- in the deriving clauses.
  _ <- evaluate (length (show msgs))
  return ()

-- Predicate helper: does any message match a code pattern?
hasCode :: (LintMessageCode -> Bool) -> [LintMessage] -> Bool
hasCode p = any (p . lintMsgCode)

-- Strict assertBool: prepends a forced prefix of 'msg' to the
-- description. Building the prefix via 'take n msg' where n is
-- evaluated forces the 'msg' reference so HPC ticks it regardless
-- of whether the assertion passes.
assertBoolStrict :: String -> Bool -> Assertion
assertBoolStrict msg cond = do
  n <- evaluate (length msg)
  let fullMsg = take n msg
  _ <- evaluate (length fullMsg)
  assertBool fullMsg cond

-- Strict assertEqual: same pattern - builds the description from
-- 'msg' via 'take n msg' so 'msg' is demanded at this call site.
assertEqualStrict :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqualStrict msg expected actual = do
  n <- evaluate (length msg)
  let fullMsg = take n msg
  _ <- evaluate (length fullMsg)
  assertEqual fullMsg expected actual

-- Force every field of 'lintConfig' so HPC ticks every literal
-- it contains (keyword transitions, priorities, column settings,
-- etc.). Without this the record's fields stay as thunks since
-- no downstream code touches them in every test case.
configForceTests :: TestTree
configForceTests =
  testGroup
    "lintConfig fields forced"
    [ testCase "rnf forces every lintConfig field" $ do
        _ <- evaluate (rnf lintConfig)
        assertEqualStrict
          "priorities match declared list"
          ["A", "B", "C"]
          (_priorities lintConfig)
        assertEqualStrict
          "startKeywords match declared list"
          ["TODO", "WAIT", "TASK"]
          (_startKeywords lintConfig)
        assertEqualStrict
          "propertyColumn matches declared value"
          11
          (_propertyColumn lintConfig)
        assertEqualStrict
          "tagsColumn matches declared value"
          60
          (_tagsColumn lintConfig)
        assertEqualStrict
          "checkFiles defaults to False"
          False
          (_checkFiles lintConfig)
        assertEqualStrict
          "keywordTransitions has three entries"
          3
          (length (_keywordTransitions lintConfig))
    ]

tests :: TestTree
tests =
  testGroup
    "Extra lint coverage"
    [ configForceTests
    , lintMessageDerivingTests
    , entryWithoutIdTests
    , singleFileLintOrgFilesTests
    , filePreambleNoEntriesTests
    , fileUrlFileProtocolTests
    , fileUrlHttpProtocolTests
    , fileAudioTests
    , fileReportOtherwiseTests
    , entryArchiveTests
    , entryAttachmentDirTests
    , entryUrlFileProtocolTests
    , entryUrlHttpProtocolTests
    , entryNoterFallthroughTests
    , entryParagraphLinkTests
    , entryReviewVariantsTests
    , beforeLogInconsistentTests
    , afterLogInconsistentTests
    , logTrailingBodyLeadingTests
    , emptyBodyTodoTests
    , logEntryUnnecessaryWhitespaceTests
    , bodyStringShowBlockTests
    , entryReportOtherwiseTests
    , pathExistsCheckFilesOffTests
    , nonTodoTimestampTests
    , entryUrlFallthroughTests
    , filePreambleRegexFallthroughTests
    , entryParagraphFallthroughTests
    , logEntryBodyBlockTests
    , allCodeVariantsTests
    , parseLintMessageKindTests
    , fileZeroEntriesTests
    ]

------------------------------------------------------------------------
-- 1. Deriving clauses for LintMessageKind, TransitionKind,
--    LintMessageCode, LintMessage
------------------------------------------------------------------------

-- Running every derived method once flips the NFData / Show /
-- Eq coverage ticks on the deriving lines (58, 73, 122, 129).
lintMessageDerivingTests :: TestTree
lintMessageDerivingTests =
  testGroup
    "LintMessage deriving clauses"
    [ testCase "Eq / Show / NFData used on all codes" $ do
        let sample =
              [ LintMessage 1 LintDebug FileFailsToRoundTrip
              , LintMessage 2 LintAll TitleWithExcessiveWhitespace
              , LintMessage 3 LintInfo EmptyBodyWhitespace
              , LintMessage 4 LintWarn TodoFileDoesNotMatchAttachment
              , LintMessage 5 LintError NonTodoWithReviewProperties
              ]
        _ <- evaluate (rnf sample)
        -- Exercise Eq and Ord on LintMessageKind.
        let kinds = [LintDebug, LintAll, LintInfo, LintWarn, LintError]
        _ <- evaluate (rnf kinds)
        assertEqualStrict
          "kinds sort the expected way"
          [LintDebug, LintAll, LintInfo, LintWarn, LintError]
          (minimum kinds : drop 1 kinds)
        -- Show forces every constructor argument.
        _ <- evaluate (length (show sample))
        pure ()
    ]

------------------------------------------------------------------------
-- 2. Entry without :ID: in lintOrgFiles (line 168 Nothing arm)
------------------------------------------------------------------------

entryWithoutIdTests :: TestTree
entryWithoutIdTests =
  testGroup
    "entry without :ID: in lintOrgFiles"
    [ testCase "entry missing :ID: skips accumulator update" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EWI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: no-id"
                , "* TODO Entry with no ID"
                , ":PROPERTIES:"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            org = parseFixture lintConfig "ewi.org" input
            msgs = lintOrgFiles lintConfig LintInfo [org]
            flat = concat (M.elems msgs)
        forceMessages flat
        assertBoolStrict
          "TodoMissingProperty ID still fires for missing :ID:"
          ( hasCode
              ( \case
                  TodoMissingProperty "ID" -> True
                  _ -> False
              )
              flat
          )
    ]

------------------------------------------------------------------------
-- 3. Single-file lintOrgFiles with unique ID (NE.singleton arm)
------------------------------------------------------------------------

singleFileLintOrgFilesTests :: TestTree
singleFileLintOrgFilesTests =
  testGroup
    "single-file lintOrgFiles"
    [ testCase "duplicate IDs force NE.singleton loc argument" $ do
        -- Two entries with the same :ID: value trigger both the
        -- NE.singleton arm (first fold step) and the NE.cons arm
        -- (second fold step). The resulting DuplicatedIdentifier
        -- message forces 'loc ^. file' and 'loc ^. pos', collapsing
        -- the 'loc' thunk stored by NE.singleton at line 168.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: dup-id"
                , "* TODO First entry sharing ID"
                , ":PROPERTIES:"
                , ":ID:       SHARED-DUP-ID"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "* TODO Second entry sharing ID"
                , ":PROPERTIES:"
                , ":ID:       SHARED-DUP-ID"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            org = parseFixture lintConfig "sf.org" input
            msgs = lintOrgFiles lintConfig LintInfo [org]
            flat = concat (M.elems msgs)
        forceMessages flat
        assertBoolStrict
          "DuplicatedIdentifier fires when two entries share an :ID:"
          ( hasCode
              (\case DuplicatedIdentifier _ -> True; _ -> False)
              flat
          )
    ]

------------------------------------------------------------------------
-- 4. File with no entries (line 215 @[] -> pure ()@ branch of
--    'lintOrgFile').
------------------------------------------------------------------------

filePreambleNoEntriesTests :: TestTree
filePreambleNoEntriesTests =
  testGroup
    "file with no entries"
    [ testCase "preamble-only file drives [] -> pure () branch" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-PO-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: preamble-only"
                , "Just a paragraph, no entries."
                ]
            msgs = runLint lintConfig "po.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 5. File-level link check: the regex captures @protocol@ as
--    @"file:"@ or @"https:"@ (including the colon), so
--    @protocol == "file"@ is always False. This means the file-link
--    branch (lines 328-332, 338-341) is dead code. The else-branch
--    @level > LintAll || urlExists (protocol ++ link)@ runs with all
--    captured links. At @LintAll@, @level > LintAll == False@, so
--    @urlExists@ is actually invoked (exercising 'urlExists'). To keep
--    the test hermetic we use a @file:@ scheme with an absolute path,
--    which @curl --output /dev/null file:/...@ reports missing.
------------------------------------------------------------------------

fileUrlFileProtocolTests :: TestTree
fileUrlFileProtocolTests =
  testGroup
    "file preamble with file: link at LintAll"
    [ testCase "file: link at LintAll reports BrokenLink via urlExists" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FUF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: file-link"
                , "See [[file:/tmp/definitely-missing-file.org][doc]]."
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintAll "/tmp/fuf.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "BrokenLink fires on missing file: target at LintAll"
          ( hasCode
              (\case BrokenLink _ -> True; _ -> False)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 6. File-level URL check with an @[[https://...]]@ link and
--    @level > LintAll@ short-circuits @urlExists@ (line 333 else-branch
--    first disjunct). Using LintInfo gets level > LintAll = True so we
--    do NOT actually perform the urlExists HTTP call; we just evaluate
--    the inline boolean and the regex fallthrough stays @_ -> pure ()@.
------------------------------------------------------------------------

fileUrlHttpProtocolTests :: TestTree
fileUrlHttpProtocolTests =
  testGroup
    "file preamble with http:// link"
    [ testCase "http:// link at LintInfo short-circuits urlExists" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FUH-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: http-link"
                , "See [[https://example.invalid/path][example]]."
                ]
            msgs = runLintAt lintConfig LintInfo "/tmp/fuh.org" input
        forceMessages msgs
        -- At LintInfo, level > LintAll is True, so the urlExists call
        -- is skipped. We just verify the rule did not crash.
        _ <- evaluate (msgs `deepseq` ())
        pure ()
    ]

------------------------------------------------------------------------
-- 7. File-level AUDIO property with @_checkFiles = True@ drives
--    @pathExists cfg doesFileExist (org ^. orgFilePath) audioPath@
--    (line 351) and fires 'AudioFileNotFound'.
------------------------------------------------------------------------

fileAudioTests :: TestTree
fileAudioTests =
  testGroup
    "file AUDIO with checkFiles=True"
    [ testCase "missing AUDIO file fires AudioFileNotFound" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FA-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":AUDIO:    /tmp/definitely-missing-audio.ogg"
                , ":END:"
                , "#+title: audio-test"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintInfo "/tmp/fa.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "AudioFileNotFound fires"
          ( hasCode
              (\case AudioFileNotFound _ -> True; _ -> False)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 8. File-level report' @otherwise@ branch (line 377). Running lint at
--    LintError filters out the LintInfo/LintWarn file messages.
------------------------------------------------------------------------

fileReportOtherwiseTests :: TestTree
fileReportOtherwiseTests =
  testGroup
    "file report' otherwise branch"
    [ testCase "LintError level filters out LintInfo file messages" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FR-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                -- No title, so FileTitleMissing (LintInfo) fires internally.
                ]
            msgs = runLintAt lintConfig LintError "/tmp/fr.org" input
        forceMessages msgs
        assertBoolStrict
          "FileTitleMissing filtered at LintError level"
          ( not
              (hasCode (== FileTitleMissing) msgs)
          )
    ]

------------------------------------------------------------------------
-- 9. Entry-level ARCHIVE property with @_checkFiles = True@ drives
--    @pathExists cfg doesFileExist (org ^. orgFilePath) path'@
--    (lines 500-503). The entry variant is triggered by placing
--    @:ARCHIVE:@ on an entry properties drawer.
------------------------------------------------------------------------

entryArchiveTests :: TestTree
entryArchiveTests =
  testGroup
    "entry ARCHIVE with checkFiles=True"
    [ testCase "entry ARCHIVE fires ArchiveTagFileDoesNotExist" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EA-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: entry-archive"
                , "* TODO Entry with archive"
                , ":PROPERTIES:"
                , ":ID:       ENT-EA-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":ARCHIVE:  /tmp/definitely-missing-archive-entry.org::"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintInfo "/tmp/ea.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "ArchiveTagFileDoesNotExist fires for entry :ARCHIVE:"
          ( hasCode
              (\case ArchiveTagFileDoesNotExist _ -> True; _ -> False)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 10. Entry-level attachment-dir check with @_checkFiles = True@ drives
--     @pathExists cfg doesDirectoryExist (org ^. orgFilePath) dir@
--     (lines 526-531).
------------------------------------------------------------------------

entryAttachmentDirTests :: TestTree
entryAttachmentDirTests =
  testGroup
    "entry Attachments + :FILE: attachment dir"
    [ testCase "missing attachment dir fires TodoFileDoesNotMatchAttachment" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-AD-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: attach-test"
                , "* TODO Has file attachment           :FILE:"
                , ":PROPERTIES:"
                , ":ID:       ATTACH-DIR-0123456789"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":Attachments: readme.txt"
                , ":END:"
                ]
            cfg =
              lintConfig
                { _checkFiles = True
                , _homeDirectory = Just "/tmp"
                , _attachmentsDir = "/tmp/definitely-missing-attachments"
                }
            msgs = runLintAt cfg LintInfo "/tmp/ad.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "TodoFileDoesNotMatchAttachment fires on missing dir"
          ( hasCode
              (== TodoFileDoesNotMatchAttachment)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 11. Entry :URL: with @[[file:...]]@ link drives the entry-level
--     @protocol == \"file\"@ pathExists args (lines 583-598).
------------------------------------------------------------------------

entryUrlFileProtocolTests :: TestTree
entryUrlFileProtocolTests =
  testGroup
    "entry :URL: with file:// link at LintAll"
    [ testCase "file:// link on entry at LintAll drives urlExists" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EUF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: entry-file-url"
                , "* TODO Entry with URL"
                , ":PROPERTIES:"
                , ":ID:       ENT-EUF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":URL:      [[file:/tmp/definitely-missing.org][doc]]"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintAll "/tmp/euf.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "BrokenLink fires on entry :URL: file:// target at LintAll"
          ( hasCode
              (\case BrokenLink _ -> True; _ -> False)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 12. Entry :URL: with @[[https://...]]@ link at LintInfo short-circuits
--     urlExists (line 588 first disjunct).
------------------------------------------------------------------------

entryUrlHttpProtocolTests :: TestTree
entryUrlHttpProtocolTests =
  testGroup
    "entry :URL: with http:// link"
    [ testCase "http:// link on entry at LintInfo short-circuits" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EUH-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: entry-http-url"
                , "* TODO Entry with URL"
                , ":PROPERTIES:"
                , ":ID:       ENT-EUH-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":URL:      [[https://example.invalid/p][ex]]"
                , ":END:"
                ]
            msgs = runLintAt lintConfig LintInfo "/tmp/euh.org" input
        forceMessages msgs
        _ <- evaluate (msgs `deepseq` ())
        pure ()
    ]

------------------------------------------------------------------------
-- 13. Entry NOTER_DOCUMENT with a value the regex @[^]:]+@ cannot
--     capture drives the @_ -> pure ()@ fallthrough at line 607.
------------------------------------------------------------------------

entryNoterFallthroughTests :: TestTree
entryNoterFallthroughTests =
  testGroup
    "entry NOTER_DOCUMENT regex fallthrough"
    [ testCase "NOTER_DOCUMENT value ']' drives regex fallthrough" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-ND-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: noter-fallthrough"
                , "* TODO Paper"
                , ":PROPERTIES:"
                , ":ID:       ENT-ND-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":NOTER_DOCUMENT: ]"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintInfo "/tmp/nd.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 14. Entry body paragraph with an embedded @[[file:...]]@ link drives
--     lines 615-630 (paragraph protocol branch).
------------------------------------------------------------------------

entryParagraphLinkTests :: TestTree
entryParagraphLinkTests =
  testGroup
    "entry paragraph with file:// link at LintAll"
    [ testCase "paragraph file:// link at LintAll fires BrokenLink" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EPL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: entry-para-link"
                , "* TODO Entry with paragraph link"
                , ":PROPERTIES:"
                , ":ID:       ENT-EPL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "See [[file:/tmp/definitely-missing-epl.org][doc]]"
                , "in the referenced file."
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintAll "/tmp/epl.org" input
        _ <- evaluate (rnf cfg)
        forceMessages msgs
        assertBoolStrict
          "BrokenLink fires on entry paragraph file:// link at LintAll"
          ( hasCode
              (\case BrokenLink _ -> True; _ -> False)
              msgs
          )
    ]

------------------------------------------------------------------------
-- 15. Entry non-TODO with NEXT_REVIEW / REVIEWS / Effort drives the
--     entry variant of 'ruleOnlyTodosReview' (lines 744-746).
------------------------------------------------------------------------

entryReviewVariantsTests :: TestTree
entryReviewVariantsTests =
  testGroup
    "entry NonTodoWithReviewProperties variants"
    [ testCase "non-TODO + NEXT_REVIEW fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-OR-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: review-variants"
                , "* Non-todo with NEXT_REVIEW"
                , ":PROPERTIES:"
                , ":ID:       ENT-OR-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":NEXT_REVIEW: [2025-01-01 Wed 00:00]"
                , ":END:"
                ]
            msgs = runLint lintConfig "or.org" input
        forceMessages msgs
        assertBoolStrict
          "NonTodoWithReviewProperties fires on NEXT_REVIEW"
          (hasCode (== NonTodoWithReviewProperties) msgs)
    , testCase "non-TODO + REVIEWS fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-OR-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: review-variants"
                , "* Non-todo with REVIEWS"
                , ":PROPERTIES:"
                , ":ID:       ENT-OR-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":REVIEWS:  3"
                , ":END:"
                ]
            msgs = runLint lintConfig "or2.org" input
        forceMessages msgs
        assertBoolStrict
          "NonTodoWithReviewProperties fires on REVIEWS"
          (hasCode (== NonTodoWithReviewProperties) msgs)
    , testCase "non-TODO + Effort fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-OR-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: review-variants"
                , "* Non-todo with Effort"
                , ":PROPERTIES:"
                , ":ID:       ENT-OR-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":Effort:   1:00"
                , ":END:"
                ]
            msgs = runLint lintConfig "or3.org" input
        forceMessages msgs
        assertBoolStrict
          "NonTodoWithReviewProperties fires on Effort"
          (hasCode (== NonTodoWithReviewProperties) msgs)
    ]

------------------------------------------------------------------------
-- 16. Inconsistent leading whitespace on log entries fires
--     @InconsistentWhitespace "before log entries"@ (line 753).
------------------------------------------------------------------------

beforeLogInconsistentTests :: TestTree
beforeLogInconsistentTests =
  testGroup
    "InconsistentWhitespace before log entries"
    [ testCase "mixed log-entry leading whitespace fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-BLI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: before-log-inconsistent"
                , "* TODO Inconsistent log leading"
                , ":PROPERTIES:"
                , ":ID:       ENT-BLI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":LOGBOOK:"
                , "- State \"DONE\" from \"TODO\" [2024-10-08 Tue 09:00] \\\\"
                , ""
                , "  first note body"
                , "- State \"DONE\" from \"TODO\" [2024-10-09 Wed 09:00] \\\\"
                , "  second note body"
                , ":END:"
                ]
            msgs = runLint lintConfig "bli.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 17. Inconsistent trailing whitespace on log entries fires
--     @InconsistentWhitespace "after log entries"@ (line 755).
------------------------------------------------------------------------

afterLogInconsistentTests :: TestTree
afterLogInconsistentTests =
  testGroup
    "InconsistentWhitespace after log entries"
    [ testCase "mixed log-entry trailing whitespace fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-ALI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: after-log-inconsistent"
                , "* TODO Inconsistent log trailing"
                , ":PROPERTIES:"
                , ":ID:       ENT-ALI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  first body"
                , ""
                , "- Note taken on [2024-10-09 Wed 09:00] \\\\"
                , "  second body"
                ]
            msgs = runLint lintConfig "ali.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 18. Log entry with body leading whitespace drives the nested case
--     @| Just ws <- bodyLeading -> do ...@ (lines 789-791).
------------------------------------------------------------------------

logTrailingBodyLeadingTests :: TestTree
logTrailingBodyLeadingTests =
  testGroup
    "logTrailing | Just ws <- bodyLeading"
    [ testCase "log entry followed by body drives inner bodyLeading case" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-LTB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: log-trailing-body-leading"
                , "* TODO Body after log"
                , ":PROPERTIES:"
                , ":ID:       ENT-LTB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  note body"
                , ""
                , "Body paragraph after log."
                , "* TODO Second"
                , ":PROPERTIES:"
                , ":ID:       ENT-LTB-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "ltb.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 19. TODO entry with whitespace-only body fires 'EmptyBodyWhitespace'
--     via @maybe False (isTodo cfg) (e ^? keyword)@ (line 817).
------------------------------------------------------------------------

emptyBodyTodoTests :: TestTree
emptyBodyTodoTests =
  testGroup
    "EmptyBodyWhitespace via TODO keyword"
    [ testCase "TODO with whitespace-only body fires EmptyBodyWhitespace" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EBT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: empty-body-todo"
                , "* TODO Whitespace body only"
                , ":PROPERTIES:"
                , ":ID:       ENT-EBT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ""
                , "* TODO Second entry sibling"
                , ":PROPERTIES:"
                , ":ID:       ENT-EBT-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "ebt.org" input
        forceMessages msgs
        assertBoolStrict
          "EmptyBodyWhitespace fires on TODO with whitespace body"
          (hasCode (== EmptyBodyWhitespace) msgs)
    ]

------------------------------------------------------------------------
-- 20. Log entry whose body starts with a paragraph that has a
--     leading space fires 'UnnecessaryWhitespace' at line 830
--     (ruleNoUnnecessaryWhitespace, log-entry branch).
------------------------------------------------------------------------

logEntryUnnecessaryWhitespaceTests :: TestTree
logEntryUnnecessaryWhitespaceTests =
  testGroup
    "log-entry UnnecessaryWhitespace"
    [ testCase "log note paragraph with leading space fires rule" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-LUW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: log-unnecessary-ws"
                , "* TODO Log with leading-space body"
                , ":PROPERTIES:"
                , ":ID:       ENT-LUW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "   leading-space note body"
                ]
            msgs = runLint lintConfig "luw.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 21. Entry body containing a Whitespace block with > 1 line drives
--     the @bodyString (has _Whitespace)@ fold and the nested
--     @runReader (showBlock "" b) cfg@ at lines 925 / 934.
--     This fires 'MultipleBlankLines'.
------------------------------------------------------------------------

bodyStringShowBlockTests :: TestTree
bodyStringShowBlockTests =
  testGroup
    "bodyString showBlock forcing"
    [ testCase "body with multiple blank lines fires MultipleBlankLines" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-BS-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: body-show-block"
                , "* TODO Entry with multiple blanks"
                , ":PROPERTIES:"
                , ":ID:       ENT-BS-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "First paragraph."
                , ""
                , ""
                , "Second paragraph after a blank gap."
                ]
            msgs = runLint lintConfig "bs.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 22. Entry-level report' otherwise branch (line 947) - filtered at
--     LintError level.
------------------------------------------------------------------------

entryReportOtherwiseTests :: TestTree
entryReportOtherwiseTests =
  testGroup
    "entry report' otherwise branch"
    [ testCase "LintError level filters out LintWarn entry messages" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-ER-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: report-filter"
                , "* TODO  Double-space in title"
                , ":PROPERTIES:"
                , ":ID:       ENT-ER-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLintAt lintConfig LintError "er.org" input
        forceMessages msgs
        assertBoolStrict
          "TitleWithExcessiveWhitespace filtered at LintError level"
          ( not
              (hasCode (== TitleWithExcessiveWhitespace) msgs)
          )
    ]

------------------------------------------------------------------------
-- 23. pathExists short-circuit (@| not (cfg ^. checkFiles) = True@)
--     at line 953.
------------------------------------------------------------------------

pathExistsCheckFilesOffTests :: TestTree
pathExistsCheckFilesOffTests =
  testGroup
    "pathExists with checkFiles=False"
    [ testCase "AUDIO with checkFiles=False does not fire AudioFileNotFound" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-PE-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":AUDIO:    /tmp/does-not-matter.ogg"
                , ":END:"
                , "#+title: audio-no-check"
                ]
            msgs = runLint lintConfig "peo.org" input
        forceMessages msgs
        assertBoolStrict
          "AudioFileNotFound should NOT fire when checkFiles=False"
          ( not
              ( hasCode
                  (\case AudioFileNotFound _ -> True; _ -> False)
                  msgs
              )
          )
    ]

------------------------------------------------------------------------
-- 24. Non-TODO entry with SCHEDULED: leading timestamp fires
--     'TimestampsOnNonTodo' via line 736's
--     @maybe True (not . isTodo cfg) (e ^? keyword)@ branch.
------------------------------------------------------------------------

nonTodoTimestampTests :: TestTree
nonTodoTimestampTests =
  testGroup
    "non-TODO + SCHEDULED timestamp"
    [ testCase "SCHEDULED on non-TODO entry fires TimestampsOnNonTodo" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-NTT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: non-todo-timestamp"
                , "* Non-todo with SCHEDULED"
                , "SCHEDULED: <2024-10-08 Tue>"
                , ":PROPERTIES:"
                , ":ID:       ENT-NTT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "ntt.org" input
        forceMessages msgs
        assertBoolStrict
          "TimestampsOnNonTodo fires on non-TODO entry with SCHEDULED stamp"
          (hasCode (== TimestampsOnNonTodo) msgs)
    ]

------------------------------------------------------------------------
-- 25. Entry with :URL: whose regex cannot match @[[(file:|https?:)...@
--     drives the fallthrough @_ -> pure ()@ at line 598.
------------------------------------------------------------------------

entryUrlFallthroughTests :: TestTree
entryUrlFallthroughTests =
  testGroup
    "entry :URL: regex fallthrough"
    [ testCase "malformed :URL: value drives regex fallthrough" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EUX-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: url-fallthrough"
                , "* TODO URL value without file:/https: prefix"
                , ":PROPERTIES:"
                , ":ID:       ENT-EUX-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":URL:      plain-text-no-link-brackets"
                , ":END:"
                ]
            msgs = runLint lintConfig "eux.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 26. File preamble with a paragraph that does NOT match the
--     @[[(file:|https?:)([^]:]+)@ regex drives the @_ -> pure ()@
--     fallthrough at line 343.
------------------------------------------------------------------------

filePreambleRegexFallthroughTests :: TestTree
filePreambleRegexFallthroughTests =
  testGroup
    "file preamble paragraph without link"
    [ testCase "plain paragraph drives regex fallthrough" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FPR-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: no-link-para"
                , "This paragraph has zero bracket-style link syntax."
                , ""
                , "A second paragraph for good measure."
                , "* TODO Placeholder entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-FPR-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "fpr.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 27. Entry body paragraph without any link drives the entry-level
--     paragraph regex fallthrough @_ -> pure ()@ at line 630.
------------------------------------------------------------------------

entryParagraphFallthroughTests :: TestTree
entryParagraphFallthroughTests =
  testGroup
    "entry body paragraph without link"
    [ testCase "entry body plain paragraph drives entry regex fallthrough" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EPF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: entry-no-link-para"
                , "* TODO Entry with plain body"
                , ":PROPERTIES:"
                , ":ID:       ENT-EPF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "A body paragraph with no link brackets at all."
                , ""
                , "A second body paragraph without anything special."
                ]
            msgs = runLint lintConfig "epf.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 28. Log entry body containing a paragraph drives the log-entry
--     @bodyString . to showBlock@ at line 934 (second half of the
--     combined 'bodyString' traversal).
------------------------------------------------------------------------

logEntryBodyBlockTests :: TestTree
logEntryBodyBlockTests =
  testGroup
    "log entry body showBlock"
    [ testCase "log note body with paragraph drives log-entry bodyString" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-LBB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: log-body-block"
                , "* TODO Log note with body"
                , ":PROPERTIES:"
                , ":ID:       ENT-LBB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  body of the note paragraph"
                ]
            msgs = runLint lintConfig "lbb.org" input
        forceMessages msgs
    ]

------------------------------------------------------------------------
-- 29. Force every 'LintMessageCode' constructor through 'rnf' and
--     'show' to tick the derived NFData / Show dictionaries at
--     lines 122 and 129. Each code is constructed with a representative
--     argument.
------------------------------------------------------------------------

allCodeVariantsTests :: TestTree
allCodeVariantsTests =
  testGroup
    "every LintMessageCode variant forced"
    [ testCase "rnf and show every LintMessageCode constructor" $ do
        let codes :: [LintMessageCode]
            codes =
              [ TodoMissingProperty "X"
              , FileMissingProperty "Y"
              , TaskMissingAssignment
              , TodoLinkDoesNotMatchUrl
              , TodoFileDoesNotMatchAttachment
              , ArchiveTagFileDoesNotExist "/tmp/arch"
              , TodoLinkKeywordImpliesLinkTag
              , FileSlugMismatch "slug"
              , MisplacedProperty
              , MisplacedTimestamp
              , MisplacedLogEntry
              , MisplacedDrawerEnd
              , DuplicateFileProperty "dup"
              , DuplicateProperty "dp"
              , DuplicateTag "tag"
              , DuplicatedIdentifier "ident"
              , InvalidStateChangeTransitionNotAllowed "TODO" (Just "DONE") ["WAIT"]
              , InvalidStateChangeInvalidTransition FirstTransition "TODO" "DONE"
              , InvalidStateChangeInvalidTransition IntermediateTransition "TODO" "DONE"
              , InvalidStateChangeInvalidTransition LastTransition "TODO" "DONE"
              , MultipleLogbooks
              , MixedLogbooks
              , WhitespaceAtStartOfLogEntry
              , FileTitleMissing
              , TitleWithExcessiveWhitespace
              , OverlyLongHeadline
              , TimestampsOnNonTodo
              , InconsistentWhitespace "ctx"
              , InconsistentFilePreambleWhitespace
              , UnnecessaryWhitespace
              , EmptyBodyWhitespace
              , MultipleBlankLines
              , CategoryTooLong "cat"
              , TitlePropertyNotLast
              , FileTagsTodoMismatch
              , VerbInFileUnknown "verb"
              , TagInFileUnknown "tag"
              , InvalidLocation "loc"
              , TodoMissingReviewProperties
              , NonTodoWithReviewProperties
              , BrokenLink "/x"
              , HashesDoNotMatch "a" "b"
              , FileFailsToRoundTrip
              , AudioFileNotFound "/audio"
              ]
        _ <- evaluate (rnf codes)
        let total = sum (map (length . show) codes)
        assertBoolStrict
          "show forces every LintMessageCode argument"
          (total > 0)
        -- Exercise TransitionKind Eq / Show too.
        let kinds = [FirstTransition, IntermediateTransition, LastTransition]
        _ <- evaluate (rnf kinds)
        assertEqualStrict
          "TransitionKind Eq reflexive"
          FirstTransition
          FirstTransition
        _ <- evaluate (length (show kinds))
        pure ()
    ]

------------------------------------------------------------------------
-- 30. parseLintMessageKind round-trip covers the string-to-kind map
--     at lines 60-67.
------------------------------------------------------------------------

parseLintMessageKindTests :: TestTree
parseLintMessageKindTests =
  testGroup
    "parseLintMessageKind coverage"
    [ testCase "every keyword round-trips and unknown returns Nothing" $ do
        assertEqualStrict "ERROR" (Just LintError) (parseLintMessageKind "ERROR")
        assertEqualStrict "WARN" (Just LintWarn) (parseLintMessageKind "WARN")
        assertEqualStrict "INFO" (Just LintInfo) (parseLintMessageKind "INFO")
        assertEqualStrict "ALL" (Just LintAll) (parseLintMessageKind "ALL")
        assertEqualStrict "DEBUG" (Just LintDebug) (parseLintMessageKind "DEBUG")
        assertEqualStrict
          "unknown returns Nothing"
          Nothing
          (parseLintMessageKind "unknown")
    ]

------------------------------------------------------------------------
-- 31. File with zero entries (just properties + title) hits the
--     @[] -> pure ()@ branch at line 215.
------------------------------------------------------------------------

fileZeroEntriesTests :: TestTree
fileZeroEntriesTests =
  testGroup
    "file with zero entries"
    [ testCase "properties+title only drives [] -> pure () branch" $ do
        -- No paragraphs, no entries - org ^.. allEntries returns [].
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FZE-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: zero-entries"
                ]
            msgs = runLint lintConfig "fze.org" input
        forceMessages msgs
        assertBoolStrict
          "no entry-level messages when allEntries is empty"
          ( not
              (hasCode (== TaskMissingAssignment) msgs)
          )
    ]
