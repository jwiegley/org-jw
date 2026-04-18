{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LintRulesTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as M
import Org.Lint
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- A configuration for use in lint tests.
--
-- Important: _checkFiles = False so that path-existence rules always pass.
-- We are testing the lint logic, not the filesystem.
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

-- Concatenation helper for building byte string fixtures from line lists.
fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

-- Parse the given bytes and run lint, returning all messages produced.
runLint :: Config -> FilePath -> ByteString -> [LintMessage]
runLint cfg = runLintAt cfg LintInfo

-- Like 'runLint' but accepts an explicit lint level. Needed for rules that
-- only fire when the level is 'LintAll' (e.g. 'BrokenLink').
runLintAt :: Config -> LintMessageKind -> FilePath -> ByteString -> [LintMessage]
runLintAt cfg level path input =
  case parseOrgFile cfg path input of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show input
    Right org -> lintOrgFile cfg level org

-- Parse multiple fixtures and run cross-file lint, returning the per-file
-- message map. Used to trigger rules that only fire across files
-- (e.g. DuplicatedIdentifier).
runLintFiles :: Config -> [(FilePath, ByteString)] -> Map FilePath [LintMessage]
runLintFiles cfg inputs =
  let parsed =
        map
          ( \(path, bs) -> case parseOrgFile cfg path bs of
              Left (_, msg) ->
                error $
                  "test fixture must parse: " ++ msg ++ "\ninput: " ++ show bs
              Right org -> org
          )
          inputs
   in lintOrgFiles cfg LintInfo parsed

-- Predicate: does this LintMessageCode match (ignoring its arguments)?
hasCode ::
  (LintMessageCode -> Bool) ->
  [LintMessage] ->
  Bool
hasCode p = any (p . lintMsgCode)

-- Assertion: at least one message has a code matching the predicate.
shouldFire ::
  String ->
  (LintMessageCode -> Bool) ->
  [LintMessage] ->
  Assertion
shouldFire desc p msgs =
  assertBool
    ("expected " ++ desc ++ " in " ++ show (map lintMsgCode msgs))
    (hasCode p msgs)

-- Assertion: no message matches the predicate.
shouldNotFire ::
  String ->
  (LintMessageCode -> Bool) ->
  [LintMessage] ->
  Assertion
shouldNotFire desc p msgs =
  assertBool
    ( "did not expect "
        ++ desc
        ++ " but got: "
        ++ show (filter (p . lintMsgCode) msgs)
    )
    (not (hasCode p msgs))

-- A bare-bones org file that should produce a clean lint pass for the
-- simple rules we don't want firing.
cleanFileBytes :: ByteString
cleanFileBytes =
  fromLines
    [ ":PROPERTIES:"
    , ":ID:       FILE-ID-001"
    , ":CREATED:  [2024-10-07 Mon 20:15]"
    , ":END:"
    , "#+title: Sample"
    ]

tests :: TestTree
tests =
  testGroup
    "Lint rules"
    [ testGroup
        "file-level rules"
        [ testCase "FileTitleMissing fires when title absent" $
            shouldFire
              "FileTitleMissing"
              (== FileTitleMissing)
              ( runLint
                  lintConfig
                  "no-title.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-002"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "FileMissingProperty ID fires when ID absent" $
            shouldFire
              "FileMissingProperty ID"
              (\c -> c == FileMissingProperty "ID")
              ( runLint
                  lintConfig
                  "no-id.org"
                  ( fromLines
                      [ "#+title: t"
                      ]
                  )
              )
        , testCase "FileMissingProperty CREATED fires when CREATED absent" $
            shouldFire
              "FileMissingProperty CREATED"
              (\c -> c == FileMissingProperty "CREATED")
              ( runLint
                  lintConfig
                  "no-created.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-003"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "TitlePropertyNotLast fires when title precedes other props" $
            shouldFire
              "TitlePropertyNotLast"
              (== TitlePropertyNotLast)
              ( runLint
                  lintConfig
                  "title-not-last.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-004"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "#+filetags: :foo:"
                      ]
                  )
              )
        , testCase "ArchiveTagFileDoesNotExist fires with checkFiles enabled" $
            shouldFire
              "ArchiveTagFileDoesNotExist"
              ( \case
                  ArchiveTagFileDoesNotExist _ -> True
                  _ -> False
              )
              ( runLint
                  ( lintConfig
                      { _checkFiles = True
                      , _homeDirectory = Just "/tmp"
                      }
                  )
                  "/tmp/archive.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-005"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":ARCHIVE:  no-such-archive-target-file.org"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "FileTagsTodoMismatch fires when filetags todo without TODOs" $
            shouldFire
              "FileTagsTodoMismatch"
              (== FileTagsTodoMismatch)
              ( runLint
                  lintConfig
                  "ft.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-006"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+filetags: :todo:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "NonTodoWithReviewProperties fires for file LAST_REVIEW" $
            shouldFire
              "NonTodoWithReviewProperties"
              (== NonTodoWithReviewProperties)
              ( runLint
                  lintConfig
                  "rev.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-007"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":LAST_REVIEW: [2024-10-01 Tue]"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "TagInFileUnknown fires for tags not in vocabulary" $
            shouldFire
              "TagInFileUnknown"
              ( \case
                  TagInFileUnknown _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "tags.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-008"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":TAGS_ALL: known1 known2"
                      , ":END:"
                      , "#+title: t"
                      , "* Heading  :unknown:"
                      ]
                  )
              )
        , testCase "VerbInFileUnknown fires for verbs not in vocabulary" $
            shouldFire
              "VerbInFileUnknown"
              ( \case
                  VerbInFileUnknown _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "verbs.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-009"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":VERB_ALL: write read"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO frob: the widget"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-V01"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "DuplicateFileProperty fires for duplicated file props" $
            shouldFire
              "DuplicateFileProperty"
              ( \case
                  DuplicateFileProperty _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "dup.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-010"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":SAME:     a"
                      , ":SAME:     b"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "FileSlugMismatch fires when filename slug differs from title" $
            shouldFire
              "FileSlugMismatch"
              ( \case
                  FileSlugMismatch _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "20240101-bad-slug.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-031"
                      , ":CREATED:  [2024-01-01 Mon 09:00]"
                      , ":END:"
                      , "#+title: Totally Different Name"
                      ]
                  )
              )
        , testCase "FileCreatedTimeMismatch fires when filename date differs from CREATED" $
            shouldFire
              "FileCreatedTimeMismatch"
              ( \case
                  FileCreatedTimeMismatch _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "20240601"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-032"
                      , ":CREATED:  [2023-01-15 Sun]"
                      , ":END:"
                      , "#+title: 20240601"
                      ]
                  )
              )
        , -- Note: the ruleCheckAllLinks rule is gated so that it effectively
          -- only fires at the LintAll level, so we use runLintAt here.
          testCase "BrokenLink fires for missing file link in preamble" $
            shouldFire
              "BrokenLink"
              ( \case
                  BrokenLink _ -> True
                  _ -> False
              )
              ( runLintAt
                  ( lintConfig
                      { _checkFiles = True
                      , _homeDirectory = Just "/tmp"
                      }
                  )
                  LintAll
                  "/tmp/broken-link.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-033"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "See [[file:/tmp/definitely-nonexistent-dir/nope.org][here]] for details."
                      ]
                  )
              )
        , testCase "AudioFileNotFound fires for missing :AUDIO: file" $
            shouldFire
              "AudioFileNotFound"
              ( \case
                  AudioFileNotFound _ -> True
                  _ -> False
              )
              ( runLint
                  ( lintConfig
                      { _checkFiles = True
                      , _homeDirectory = Just "/tmp"
                      }
                  )
                  "/tmp/audio.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-034"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":AUDIO:    /tmp/definitely-nonexistent-audio.ogg"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        , testCase "Clean file produces no error-level messages" $
            shouldNotFire
              "any error"
              (\c -> case c of FileTitleMissing -> True; _ -> False)
              ( runLint
                  lintConfig
                  "clean.org"
                  cleanFileBytes
              )
        ]
    , testGroup
        "entry-level rules"
        [ testCase "TodoMissingProperty ID fires for TODO without ID" $
            shouldFire
              "TodoMissingProperty ID"
              (\c -> c == TodoMissingProperty "ID")
              ( runLint
                  lintConfig
                  "todo-noid.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-011"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      ]
                  )
              )
        , testCase "TodoMissingProperty CREATED fires for TODO without CREATED" $
            shouldFire
              "TodoMissingProperty CREATED"
              (\c -> c == TodoMissingProperty "CREATED")
              ( runLint
                  lintConfig
                  "todo-nocreated.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-012"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-001"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TaskMissingAssignment fires for TASK without tags" $
            shouldFire
              "TaskMissingAssignment"
              (== TaskMissingAssignment)
              ( runLint
                  lintConfig
                  "task.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-013"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TASK Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-002"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "CategoryTooLong fires for long CATEGORY" $
            shouldFire
              "CategoryTooLong"
              ( \case
                  CategoryTooLong _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "cat.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-014"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-003"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":CATEGORY: ThisCategoryIsWayTooLong"
                      , ":END:"
                      ]
                  )
              )
        , testCase "MisplacedProperty fires for :PROPERTIES: in body" $
            shouldFire
              "MisplacedProperty"
              (== MisplacedProperty)
              ( runLint
                  lintConfig
                  "mp.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-015"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-004"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "Body text mentioning :PROPERTIES: in passing"
                      ]
                  )
              )
        , testCase "MisplacedTimestamp fires for stamp in body" $
            shouldFire
              "MisplacedTimestamp"
              (== MisplacedTimestamp)
              ( runLint
                  lintConfig
                  "mt.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-016"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-005"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "SCHEDULED: <2024-10-09 Wed>"
                      ]
                  )
              )
        , testCase "MisplacedLogEntry fires for log marker in body" $
            shouldFire
              "MisplacedLogEntry"
              (== MisplacedLogEntry)
              ( runLint
                  lintConfig
                  "mle.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-017"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-006"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "- CLOSING NOTE [2024-10-08 Tue 09:00] \\\\"
                      , "  some note"
                      ]
                  )
              )
        , testCase "MisplacedDrawerEnd fires for stray :END: in body" $
            shouldFire
              "MisplacedDrawerEnd"
              (== MisplacedDrawerEnd)
              ( runLint
                  lintConfig
                  "mde.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-018"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-007"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "stray :END:"
                      ]
                  )
              )
        , testCase "DuplicateTag fires for duplicated tag" $
            shouldFire
              "DuplicateTag"
              ( \case
                  DuplicateTag _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "dt.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-019"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Heading  :foo:foo:"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-008"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "DuplicateProperty fires for duplicated property" $
            shouldFire
              "DuplicateProperty"
              ( \case
                  DuplicateProperty _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "dp.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-020"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-009"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":SAME:     a"
                      , ":SAME:     b"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TitleWithExcessiveWhitespace fires for double space in title" $
            shouldFire
              "TitleWithExcessiveWhitespace"
              (== TitleWithExcessiveWhitespace)
              ( runLint
                  lintConfig
                  "ws.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-021"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Two  spaces in title"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-010"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "InvalidLocation fires for 0,0 location" $
            shouldFire
              "InvalidLocation"
              ( \case
                  InvalidLocation _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "loc.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-022"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-011"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":LOCATION: 0.0,0.0"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TodoFileDoesNotMatchAttachment fires for Attachments without :FILE: tag" $
            shouldFire
              "TodoFileDoesNotMatchAttachment"
              (== TodoFileDoesNotMatchAttachment)
              ( runLint
                  lintConfig
                  "att.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-023"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-012"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":Attachments: file1.pdf"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TodoLinkDoesNotMatchUrl fires for LINK keyword without URL" $
            shouldFire
              "TodoLinkDoesNotMatchUrl"
              (== TodoLinkDoesNotMatchUrl)
              ( runLint
                  lintConfig
                  "lk.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-024"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* LINK Some thing"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-013"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TodoLinkKeywordImpliesLinkTag fires for LINK keyword with :LINK: tag" $
            shouldFire
              "TodoLinkKeywordImpliesLinkTag"
              (== TodoLinkKeywordImpliesLinkTag)
              ( runLint
                  lintConfig
                  "ll.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-025"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* LINK Some thing  :LINK:"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-014"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":URL:      https://example.com"
                      , ":END:"
                      ]
                  )
              )
        , testCase "InvalidStateChangeIdempotent fires for self-loop transition" $
            shouldFire
              "InvalidStateChangeIdempotent"
              ( \case
                  InvalidStateChangeIdempotent _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "isc.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-026"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Self transition"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-015"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "- State \"TODO\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                      ]
                  )
              )
        , testCase "EmptyBodyWhitespace fires for whitespace-only body on TODO" $
            shouldFire
              "EmptyBodyWhitespace"
              (== EmptyBodyWhitespace)
              ( runLint
                  lintConfig
                  "ebw.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-027"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-016"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , ""
                      ]
                  )
              )
        , testCase "InvalidDrawerCase fires for mixed-case PlainDrawer" $
            shouldFire
              "InvalidDrawerCase"
              ( \case
                  InvalidDrawerCase _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "idc.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-028"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Some task"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-017"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ":Notes:"
                      , "some content"
                      , ":END:"
                      ]
                  )
              )
        , testCase "TimestampsOnNonTodo fires for non-TODO with SCHEDULED" $
            shouldFire
              "TimestampsOnNonTodo"
              (== TimestampsOnNonTodo)
              ( runLint
                  lintConfig
                  "tont.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-029"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* Plain heading"
                      , "SCHEDULED: <2024-10-09 Wed>"
                      ]
                  )
              )
        , testCase "NonTodoWithReviewProperties fires for plain entry with LAST_REVIEW" $
            shouldFire
              "NonTodoWithReviewProperties"
              (== NonTodoWithReviewProperties)
              ( runLint
                  lintConfig
                  "ntr.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-030"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* Plain heading"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-018"
                      , ":LAST_REVIEW: [2024-10-01 Tue]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "MultipleLogbooks fires for two LOGBOOK drawers" $
            shouldFire
              "MultipleLogbooks"
              (== MultipleLogbooks)
              ( runLint
                  lintConfig
                  "mlb.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-031"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO With two logbooks"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-019"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ":LOGBOOK:"
                      , "CLOCK: [2024-10-08 Tue 09:00]--[2024-10-08 Tue 10:00] =>  1:00"
                      , ":END:"
                      , ":LOGBOOK:"
                      , "CLOCK: [2024-10-09 Wed 09:00]--[2024-10-09 Wed 10:00] =>  1:00"
                      , ":END:"
                      ]
                  )
              )
        , testCase "MixedLogbooks fires when log entries live inside and outside LOGBOOK" $
            shouldFire
              "MixedLogbooks"
              (== MixedLogbooks)
              ( runLint
                  lintConfig
                  "mixlb.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-032"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Mixed log layout"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-020"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "- Note taken on [2024-10-08 Tue 10:00]"
                      , ":LOGBOOK:"
                      , "- Note taken on [2024-10-09 Wed 10:00]"
                      , ":END:"
                      ]
                  )
              )
        , -- Note: 'MultipleBlankLines' is not currently reachable by the parser
          -- output. 'parseWhitespaceBlock' stores only the leading spaces from
          -- a blank line and never the newline itself, so the text of any
          -- 'Whitespace' block never has more than zero newlines, and the
          -- rule's 'lines' check never exceeds one element. If the parser ever
          -- changes to embed newlines in Whitespace, add a fixture here that
          -- produces a single Whitespace block with multi-line text. The rule
          -- is exercised via the 'ShowLintOrgTest' formatting tests.
          testCase "UnnecessaryWhitespace fires for leading space on body line" $
            shouldFire
              "UnnecessaryWhitespace"
              (== UnnecessaryWhitespace)
              ( runLint
                  lintConfig
                  "uw.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-034"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Leading space"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-022"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , " leading space on first line"
                      ]
                  )
              )
        , testCase "InconsistentWhitespace fires when body leading/trailing differ" $
            shouldFire
              "InconsistentWhitespace"
              ( \case
                  InconsistentWhitespace _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "iws.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-035"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO First"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-023"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "Some text"
                      , "* TODO Second"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-024"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "InvalidStateChangeWrongTimeOrder fires for out-of-order log times" $
            shouldFire
              "InvalidStateChangeWrongTimeOrder"
              ( \case
                  InvalidStateChangeWrongTimeOrder _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "iscwto.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-036"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* WAIT Out of order"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-025"
                      , ":CREATED:  [2024-10-01 Tue 10:00]"
                      , ":END:"
                      , "- State \"DONE\"       from \"TODO\"       [2024-10-01 Tue 10:00]"
                      , "- State \"WAIT\"       from \"DONE\"       [2024-10-05 Sat 10:00]"
                      ]
                  )
              )
        , testCase "InvalidStateChangeInvalidTransition LastTransition fires for mismatched final state" $
            shouldFire
              "InvalidStateChangeInvalidTransition LastTransition"
              ( \case
                  InvalidStateChangeInvalidTransition LastTransition _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "isclt.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-037"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Last mismatch"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-026"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "- State \"DONE\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                      ]
                  )
              )
        , testCase "InvalidStateChangeTransitionNotAllowed fires for disallowed target" $
            shouldFire
              "InvalidStateChangeTransitionNotAllowed"
              ( \case
                  InvalidStateChangeTransitionNotAllowed{} -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "isctna.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-038"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* WAIT Bad transition"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-027"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "- State \"WAIT\"       from \"TASK\"       [2024-10-08 Tue 09:00]"
                      ]
                  )
              )
        , testCase "WhitespaceAtStartOfLogEntry fires for blank-line start of note body" $
            shouldFire
              "WhitespaceAtStartOfLogEntry"
              (== WhitespaceAtStartOfLogEntry)
              ( runLint
                  lintConfig
                  "wsle.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-039"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Log note with blank start"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-028"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ":LOGBOOK:"
                      , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                      , ""
                      , "  body content"
                      , ":END:"
                      ]
                  )
              )
        ]
    , testGroup
        "cross-file rules"
        [ testCase "DuplicatedIdentifier fires when two files share an ID" $
            let msgs =
                  runLintFiles
                    lintConfig
                    [
                      ( "dup-a.org"
                      , fromLines
                          [ ":PROPERTIES:"
                          , ":ID:       FILE-ID-040"
                          , ":CREATED:  [2024-10-07 Mon 20:15]"
                          , ":END:"
                          , "#+title: a"
                          , "* TODO Shared"
                          , ":PROPERTIES:"
                          , ":ID:       SHARED-ID-001"
                          , ":CREATED:  [2024-10-07 Mon 20:15]"
                          , ":END:"
                          ]
                      )
                    ,
                      ( "dup-b.org"
                      , fromLines
                          [ ":PROPERTIES:"
                          , ":ID:       FILE-ID-041"
                          , ":CREATED:  [2024-10-07 Mon 20:15]"
                          , ":END:"
                          , "#+title: b"
                          , "* TODO Shared"
                          , ":PROPERTIES:"
                          , ":ID:       SHARED-ID-001"
                          , ":CREATED:  [2024-10-07 Mon 20:15]"
                          , ":END:"
                          ]
                      )
                    ]
                allMsgs = concat (M.elems msgs)
             in shouldFire
                  "DuplicatedIdentifier"
                  ( \case
                      DuplicatedIdentifier _ -> True
                      _ -> False
                  )
                  allMsgs
        ]
    ]
