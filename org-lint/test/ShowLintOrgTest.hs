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

tests :: TestTree
tests =
  testGroup
    "showLintOrg formatting"
    [ testCase "all codes format to a nonempty string" $
        mapM_
          ( \code ->
              assertBool (show code) (not (null (formatted code)))
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
    ]
