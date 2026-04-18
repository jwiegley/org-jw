module ShowLintOrgTest (tests) where

import Data.List (isInfixOf)
import Org.Lint
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

sampleTime :: Time
sampleTime =
  Time
    { _timeKind = ActiveTime
    , _timeDay = 60000
    , _timeDayEnd = Nothing
    , _timeStart = Nothing
    , _timeEnd = Nothing
    , _timeSuffix = Nothing
    }

-- Every constructor of LintMessageCode. If a new constructor is added to
-- Org.Lint, append it here so the formatter keeps full coverage.
allCodes :: [LintMessageCode]
allCodes =
  [ TodoMissingProperty "ID"
  , FileMissingProperty "TITLE"
  , TaskMissingAssignment
  , TodoLinkDoesNotMatchUrl
  , TodoFileDoesNotMatchAttachment
  , ArchiveTagFileDoesNotExist "/tmp/missing.org"
  , TodoLinkKeywordImpliesLinkTag
  , FileSlugMismatch "right-slug"
  , MisplacedProperty
  , MisplacedTimestamp
  , MisplacedLogEntry
  , MisplacedDrawerEnd
  , DuplicateFileProperty "ID"
  , DuplicateProperty "ID"
  , DuplicateTag "work"
  , DuplicatedIdentifier "abc-123"
  , InvalidStateChangeTransitionNotAllowed "DONE" (Just "TODO") ["WAIT"]
  , InvalidStateChangeInvalidTransition FirstTransition "TODO" "DONE"
  , InvalidStateChangeInvalidTransition IntermediateTransition "TODO" "DONE"
  , InvalidStateChangeInvalidTransition LastTransition "TODO" "DONE"
  , InvalidStateChangeWrongTimeOrder sampleTime sampleTime
  , InvalidStateChangeIdempotent "TODO"
  , MultipleLogbooks
  , MixedLogbooks
  , WhitespaceAtStartOfLogEntry
  , FileTitleMissing
  , TitleWithExcessiveWhitespace
  , OverlyLongHeadline
  , TimestampsOnNonTodo
  , InconsistentWhitespace "around drawer"
  , InconsistentFilePreambleWhitespace
  , UnnecessaryWhitespace
  , EmptyBodyWhitespace
  , MultipleBlankLines
  , CategoryTooLong "WayTooLongCategoryName"
  , FileCreatedTimeMismatch sampleTime sampleTime
  , TitlePropertyNotLast
  , FileTagsTodoMismatch
  , VerbInFileUnknown "FROB"
  , TagInFileUnknown "custom"
  , InvalidLocation "nowhere"
  , InvalidDrawerCase (PlainDrawer "Logbook")
  , InvalidDrawerCase (BeginDrawer "Properties")
  , TodoMissingReviewProperties
  , NonTodoWithReviewProperties
  , BrokenLink "file:missing.org"
  , HashesDoNotMatch "aaaa" "bbbb"
  , FileFailsToRoundTrip
  , AudioFileNotFound "/tmp/no-audio.ogg"
  ]

formatted :: LintMessageCode -> String
formatted code = showLintOrg "/tmp/file.org" (LintMessage 1 LintWarn code)

-- 'length' is used (instead of 'null' or similar) to force full evaluation
-- of the formatted message. This matters for HPC coverage: we want to make
-- sure each constructor arm of 'showLintOrg's inner case-on-code is evaluated.
{-# ANN isFullyFormatted ("HLint: ignore Use null" :: String) #-}
isFullyFormatted :: LintMessageCode -> Bool
isFullyFormatted code = length (formatted code) > 0

tests :: TestTree
tests =
  testGroup
    "showLintOrg formatting"
    [ testCase "all codes format to a nonempty string" $
        mapM_
          ( \code ->
              assertBool (show code) (isFullyFormatted code)
          )
          allCodes
    , testCase "prefix includes file and line" $
        assertBool
          "prefix"
          ("/tmp/file.org:1:" `isInfixOf` formatted FileFailsToRoundTrip)
    , testCase "kind token rendered" $
        assertBool "kind" ("WARN" `isInfixOf` formatted FileFailsToRoundTrip)
    , testCase "ERROR kind rendered" $
        assertBool "error" $
          "ERROR"
            `isInfixOf` showLintOrg
              "/tmp/file.org"
              (LintMessage 1 LintError FileFailsToRoundTrip)
    , testCase "INFO kind rendered" $
        assertBool "info" $
          "INFO"
            `isInfixOf` showLintOrg
              "/tmp/file.org"
              (LintMessage 1 LintInfo FileFailsToRoundTrip)
    , testCase "ALL kind rendered" $
        assertBool "all" $
          "ALL"
            `isInfixOf` showLintOrg
              "/tmp/file.org"
              (LintMessage 1 LintAll FileFailsToRoundTrip)
    , testCase "DEBUG kind rendered" $
        assertBool "debug" $
          "DEBUG"
            `isInfixOf` showLintOrg
              "/tmp/file.org"
              (LintMessage 1 LintDebug FileFailsToRoundTrip)
    , testGroup
        "each code formats to a distinct, descriptive message"
        [ testCase "FileSlugMismatch mentions git mv" $
            assertBool
              "slug"
              ("git mv" `isInfixOf` formatted (FileSlugMismatch "new-slug"))
        , testCase "TodoMissingProperty mentions property name" $
            assertBool
              "prop"
              ( "\"PROP\""
                  `isInfixOf` formatted (TodoMissingProperty "PROP")
              )
        , testCase "FileMissingProperty mentions property name" $
            assertBool
              "fprop"
              ( "\"FPROP\""
                  `isInfixOf` formatted (FileMissingProperty "FPROP")
              )
        , testCase "TaskMissingAssignment mentions task" $
            assertBool
              "task"
              ("Task" `isInfixOf` formatted TaskMissingAssignment)
        , testCase "TodoLinkDoesNotMatchUrl mentions URL" $
            assertBool
              "link"
              ("URL" `isInfixOf` formatted TodoLinkDoesNotMatchUrl)
        , testCase "TodoFileDoesNotMatchAttachment mentions FILE" $
            assertBool
              "file"
              ( ":FILE:"
                  `isInfixOf` formatted TodoFileDoesNotMatchAttachment
              )
        , testCase "ArchiveTagFileDoesNotExist mentions path" $
            assertBool
              "arch"
              ( "xyz.org"
                  `isInfixOf` formatted (ArchiveTagFileDoesNotExist "xyz.org")
              )
        , testCase "TodoLinkKeywordImpliesLinkTag mentions LINK" $
            assertBool
              "link-kw"
              ("LINK" `isInfixOf` formatted TodoLinkKeywordImpliesLinkTag)
        , testCase "MisplacedProperty mentions PROPERTIES" $
            assertBool
              "mp"
              (":PROPERTIES:" `isInfixOf` formatted MisplacedProperty)
        , testCase "MisplacedTimestamp mentions SCHEDULED" $
            assertBool
              "mt"
              ("SCHEDULED" `isInfixOf` formatted MisplacedTimestamp)
        , testCase "MisplacedLogEntry mentions log" $
            assertBool
              "mle"
              ("log" `isInfixOf` formatted MisplacedLogEntry)
        , testCase "MisplacedDrawerEnd mentions drawer" $
            assertBool
              "mde"
              ("drawer" `isInfixOf` formatted MisplacedDrawerEnd)
        , testCase "WhitespaceAtStartOfLogEntry mentions Log entry" $
            assertBool
              "wsle"
              ("Log entry" `isInfixOf` formatted WhitespaceAtStartOfLogEntry)
        , testCase "FileTitleMissing mentions Title" $
            assertBool
              "title"
              ("Title" `isInfixOf` formatted FileTitleMissing)
        , testCase "TitleWithExcessiveWhitespace mentions whitespace" $
            assertBool
              "twew"
              ( "whitespace"
                  `isInfixOf` formatted TitleWithExcessiveWhitespace
              )
        , testCase "OverlyLongHeadline mentions long" $
            assertBool
              "olh"
              ("long" `isInfixOf` formatted OverlyLongHeadline)
        , testCase "DuplicateFileProperty mentions name" $
            assertBool
              "dfp"
              ( "\"NAME\""
                  `isInfixOf` formatted (DuplicateFileProperty "NAME")
              )
        , testCase "DuplicateProperty mentions name" $
            assertBool
              "dp"
              ( "\"NAME\""
                  `isInfixOf` formatted (DuplicateProperty "NAME")
              )
        , testCase "DuplicateTag mentions tag" $
            assertBool
              "dt"
              ( "\"TAG\""
                  `isInfixOf` formatted (DuplicateTag "TAG")
              )
        , testCase "DuplicatedIdentifier mentions id" $
            assertBool
              "di"
              ( "id-001"
                  `isInfixOf` formatted (DuplicatedIdentifier "id-001")
              )
        , testCase "InvalidStateChangeTransitionNotAllowed mentions arrow" $
            assertBool
              "isctna"
              ( "->"
                  `isInfixOf` formatted
                    ( InvalidStateChangeTransitionNotAllowed
                        "X"
                        (Just "Y")
                        ["Z"]
                    )
              )
        , testCase "InvalidStateChangeInvalidTransition FirstTransition says initial" $
            assertBool
              "first"
              ( "initial"
                  `isInfixOf` formatted
                    (InvalidStateChangeInvalidTransition FirstTransition "A" "B")
              )
        , testCase "InvalidStateChangeInvalidTransition IntermediateTransition says intermediate" $
            assertBool
              "inter"
              ( "intermediate"
                  `isInfixOf` formatted
                    (InvalidStateChangeInvalidTransition IntermediateTransition "A" "B")
              )
        , testCase "InvalidStateChangeInvalidTransition LastTransition says final" $
            assertBool
              "last"
              ( "final"
                  `isInfixOf` formatted
                    (InvalidStateChangeInvalidTransition LastTransition "A" "B")
              )
        , testCase "InvalidStateChangeWrongTimeOrder mentions log" $
            assertBool
              "iscwto"
              ( "log"
                  `isInfixOf` formatted
                    (InvalidStateChangeWrongTimeOrder sampleTime sampleTime)
              )
        , testCase "InvalidStateChangeIdempotent mentions Idempotent" $
            assertBool
              "isci"
              ( "Idempotent"
                  `isInfixOf` formatted (InvalidStateChangeIdempotent "K")
              )
        , testCase "MultipleLogbooks mentions logbooks" $
            assertBool
              "mlb"
              ("logbooks" `isInfixOf` formatted MultipleLogbooks)
        , testCase "MixedLogbooks mentions logbooks" $
            assertBool
              "mixlb"
              ("logbooks" `isInfixOf` formatted MixedLogbooks)
        , testCase "TimestampsOnNonTodo mentions non-todo" $
            assertBool
              "tont"
              ("non-todo" `isInfixOf` formatted TimestampsOnNonTodo)
        , testCase "InconsistentWhitespace mentions description" $
            assertBool
              "iws"
              ( "describe me"
                  `isInfixOf` formatted (InconsistentWhitespace "describe me")
              )
        , testCase "InconsistentFilePreambleWhitespace mentions preamble" $
            assertBool
              "ifpw"
              ( "preamble"
                  `isInfixOf` formatted InconsistentFilePreambleWhitespace
              )
        , testCase "EmptyBodyWhitespace mentions body" $
            assertBool
              "ebw"
              ("body" `isInfixOf` formatted EmptyBodyWhitespace)
        , testCase "UnnecessaryWhitespace mentions Unnecessary" $
            assertBool
              "uw"
              ("Unnecessary" `isInfixOf` formatted UnnecessaryWhitespace)
        , testCase "MultipleBlankLines mentions blank lines" $
            assertBool
              "mbl"
              ("blank lines" `isInfixOf` formatted MultipleBlankLines)
        , testCase "CategoryTooLong mentions category name" $
            assertBool
              "ctl"
              ( "BigCategory"
                  `isInfixOf` formatted (CategoryTooLong "BigCategory")
              )
        , testCase "FileCreatedTimeMismatch mentions does not match" $
            assertBool
              "fctm"
              ( "does not match"
                  `isInfixOf` formatted
                    (FileCreatedTimeMismatch sampleTime sampleTime)
              )
        , testCase "TitlePropertyNotLast mentions last" $
            assertBool
              "tpnl"
              ("last" `isInfixOf` formatted TitlePropertyNotLast)
        , testCase "FileTagsTodoMismatch mentions Filetags" $
            assertBool
              "fttm"
              ("Filetags" `isInfixOf` formatted FileTagsTodoMismatch)
        , testCase "TagInFileUnknown mentions tag" $
            assertBool
              "tifu"
              ( "weirdtag"
                  `isInfixOf` formatted (TagInFileUnknown "weirdtag")
              )
        , testCase "VerbInFileUnknown mentions verb" $
            assertBool
              "vifu"
              ( "weirdverb"
                  `isInfixOf` formatted (VerbInFileUnknown "weirdverb")
              )
        , testCase "InvalidLocation mentions location" $
            assertBool
              "il"
              ( "somewhere"
                  `isInfixOf` formatted (InvalidLocation "somewhere")
              )
        , testCase "InvalidDrawerCase PlainDrawer mentions drawer" $
            assertBool
              "idcp"
              ( "PlainDrawer"
                  `isInfixOf` formatted (InvalidDrawerCase (PlainDrawer "Foo"))
              )
        , testCase "InvalidDrawerCase BeginDrawer mentions drawer" $
            assertBool
              "idcb"
              ( "BeginDrawer"
                  `isInfixOf` formatted (InvalidDrawerCase (BeginDrawer "Foo"))
              )
        , testCase "TodoMissingReviewProperties mentions REVIEW" $
            assertBool
              "tmrp"
              ( "REVIEW"
                  `isInfixOf` formatted TodoMissingReviewProperties
              )
        , testCase "NonTodoWithReviewProperties mentions REVIEW" $
            assertBool
              "ntrp"
              ( "REVIEW"
                  `isInfixOf` formatted NonTodoWithReviewProperties
              )
        , testCase "BrokenLink mentions link" $
            assertBool
              "bl"
              ( "some-link"
                  `isInfixOf` formatted (BrokenLink "some-link")
              )
        , testCase "HashesDoNotMatch mentions both hashes" $
            assertBool
              "hdnm"
              ( "aa"
                  `isInfixOf` formatted (HashesDoNotMatch "aa" "bb")
              )
        , testCase "FileFailsToRoundTrip mentions round trip" $
            assertBool
              "fftrt"
              ( "round trip"
                  `isInfixOf` formatted FileFailsToRoundTrip
              )
        , testCase "AudioFileNotFound mentions AUDIO" $
            assertBool
              "afnf"
              ( "AUDIO"
                  `isInfixOf` formatted (AudioFileNotFound "/tmp/a.ogg")
              )
        ]
    ]
