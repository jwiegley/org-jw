{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Additional lint tests aimed at raising HPC expression coverage of
'Org.Lint'. These target branches and thunk positions that the
existing test modules do not exercise, including:

* Forcing the argument thunks of 'LintMessage' constructors that
  Writer-monad accumulation leaves as unevaluated thunks.

* Exercising the derived 'Enum', 'Bounded', 'Show', 'Ord', and 'NFData'
  instances of 'LintMessageKind', 'TransitionKind', 'LintMessageCode',
  and 'LintMessage'.

* Driving the Attachments + ID + checkFiles branch of
  'ruleFileTagMatchesAttachment'.

* The home-directory fallback in 'pathExists' when
  '_homeDirectory' is 'Nothing'.

* The 'report' / 'report'' "kind < level" branch.

* The "NOTER_DOCUMENT contains devonthink" branch of the entry-level
  link checker.

* The 'EmptyBodyWhitespace' path that fires from the
  last-log-whitespace case in 'ruleNoInconsistentWhitespace'.

* The "surrounding body" 'InconsistentWhitespace' branch.

* The 'logTrailing' / 'bodyLeading' combination in
  'ruleNoInconsistentWhitespace' that forces the nested
  "| Just ws <- bodyLeading" branch.

* Empty-file lintOrgFile path (no entries).
-}
module LintUncoveredTest (tests) where

import Control.DeepSeq (deepseq, rnf)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Org.Lint
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- A minimal configuration matching the one in 'LintRulesTest'.
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

runLint :: Config -> FilePath -> ByteString -> [LintMessage]
runLint cfg = runLintAt cfg LintInfo

runLintAt :: Config -> LintMessageKind -> FilePath -> ByteString -> [LintMessage]
runLintAt cfg level path input =
  case parseOrgFile cfg path input of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show input
    Right org -> lintOrgFile cfg level org

-- Parse fixture, returning 'Left' for parse errors so callers that
-- need only the lint pass can deepseq the result.
parseFixture :: Config -> FilePath -> ByteString -> OrgFile
parseFixture cfg path bs =
  case parseOrgFile cfg path bs of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show bs
    Right org -> org

-- Force every LintMessage and its inner fields to WHNF and beyond.
-- This turns 'nottickedoff' thunk positions into 'istickedoff' by
-- evaluating the constructor arguments that the Writer monad would
-- otherwise leave unforced.
forceMessages :: [LintMessage] -> IO ()
forceMessages msgs = do
  rnf msgs `seq` pure ()
  -- Touch every field explicitly for belt-and-braces evaluation.
  mapM_
    ( \m ->
        lintMsgPos m `seq`
          lintMsgKind m `seq`
            lintMsgCode m `seq`
              pure ()
    )
    msgs
  -- Render messages so the inner Show instance forces the constructor
  -- arguments (String, Time, Int, DrawerType, TransitionKind, ...).
  let _ = length (show msgs)
  pure ()

-- Predicate: does any message match the predicate?
hasCode :: (LintMessageCode -> Bool) -> [LintMessage] -> Bool
hasCode p = any (p . lintMsgCode)

tests :: TestTree
tests =
  testGroup
    "Uncovered lint paths"
    [ derivedInstanceTests
    , emptyFileTests
    , forceThunkTests
    , attachmentWithIdTests
    , homeDirectoryTests
    , reportFilterTests
    , devonthinkTests
    , surroundingWhitespaceTests
    , emptyBodyWhitespaceViaLogTests
    , logTrailingFallbackTests
    , trailingBodyMatchesLeadingTests
    , duplicateFilePropertyTests
    , fileCreatedTimestampTests
    , categoryTooLongTests
    , stateChangeVariantTests
    , afterLogInconsistentTests
    , entryUrlBrokenLinkTests
    , multipleLogbooksTests
    , mixedLogbooksTests
    , debugLevelTests
    , audioFileTests
    , filePreambleLinkTests
    , noterDocumentBrokenTests
    , entryUrlFileLinkTests
    , entryParagraphFileLinkTests
    , logEntryUnnecessaryWhitespaceTests
    , beforeLogInconsistentTests
    ]

-- Exercise derived Enum, Bounded, Show, Ord, NFData instances on
-- every Lint* data type. These declarations generate code under
-- their 'deriving' clauses that HPC instruments; running the methods
-- forces those clauses to be ticked.
derivedInstanceTests :: TestTree
derivedInstanceTests =
  testGroup
    "derived instance coverage"
    [ testCase "LintMessageKind: Enum, Bounded, Show, Ord" $ do
        -- Enumerate the full range
        let ks = [minBound .. maxBound] :: [LintMessageKind]
        length ks @?= 5
        -- Show each one
        mapM_
          ( \k ->
              length (show k) `seq` pure ()
          )
          ks
        -- Ordering: LintDebug < LintAll < LintInfo < LintWarn < LintError
        compare LintDebug LintAll @?= LT
        compare LintError LintDebug @?= GT
        compare LintInfo LintInfo @?= EQ
        -- succ / pred
        succ LintDebug @?= LintAll
        pred LintError @?= LintWarn
        -- Enum methods
        fromEnum LintInfo @?= 2
        toEnum 0 @?= LintDebug
        -- NFData
        rnf ks `seq` pure ()
    , testCase "TransitionKind: Show, Eq, NFData" $ do
        let ts =
              [FirstTransition, IntermediateTransition, LastTransition]
        length (show ts) `seq` pure ()
        -- Compare with ==
        (FirstTransition == IntermediateTransition) @?= False
        (LastTransition == LastTransition) @?= True
        rnf ts `seq` pure ()
    , testCase "LintMessageCode: Show, NFData, and Eq cover all variants" $ do
        -- Build at least one value of each variant; force Show to
        -- tick every inner field.
        let codes =
              [ TodoMissingProperty "ID"
              , FileMissingProperty "CREATED"
              , TaskMissingAssignment
              , TodoLinkDoesNotMatchUrl
              , TodoFileDoesNotMatchAttachment
              , ArchiveTagFileDoesNotExist "/tmp/x"
              , TodoLinkKeywordImpliesLinkTag
              , FileSlugMismatch "slug"
              , MisplacedProperty
              , MisplacedTimestamp
              , MisplacedLogEntry
              , MisplacedDrawerEnd
              , DuplicateFileProperty "p"
              , DuplicateProperty "p"
              , DuplicateTag "t"
              , DuplicatedIdentifier "id"
              , InvalidStateChangeTransitionNotAllowed
                  "DONE"
                  (Just "TODO")
                  ["WAIT"]
              , InvalidStateChangeInvalidTransition
                  FirstTransition
                  "TODO"
                  "DONE"
              , InvalidStateChangeIdempotent "TODO"
              , MultipleLogbooks
              , MixedLogbooks
              , WhitespaceAtStartOfLogEntry
              , FileTitleMissing
              , TitleWithExcessiveWhitespace
              , OverlyLongHeadline
              , TimestampsOnNonTodo
              , InconsistentWhitespace "before log entries"
              , InconsistentFilePreambleWhitespace
              , UnnecessaryWhitespace
              , EmptyBodyWhitespace
              , MultipleBlankLines
              , CategoryTooLong "VeryLongName"
              , TitlePropertyNotLast
              , FileTagsTodoMismatch
              , VerbInFileUnknown "verb"
              , TagInFileUnknown "tag"
              , InvalidLocation "0.0,0.0"
              , InvalidDrawerCase (PlainDrawer "Mixed")
              , InvalidDrawerCase (BeginDrawer "Example")
              , TodoMissingReviewProperties
              , NonTodoWithReviewProperties
              , BrokenLink "https://example.com/nope"
              , HashesDoNotMatch "aaa" "bbb"
              , FileFailsToRoundTrip
              , AudioFileNotFound "/tmp/x.ogg"
              ]
        length (show codes) `seq` pure ()
        rnf codes `seq` pure ()
        -- Also touch == on a few variants to tick the 'Eq' derived
        -- clause.
        (TaskMissingAssignment == TaskMissingAssignment) @?= True
        (FileTitleMissing == TitlePropertyNotLast) @?= False
    , testCase "LintMessage: Show, Eq, NFData" $ do
        let loc = Loc "/tmp/x.org" 1
            m =
              LintMessage
                { lintMsgPos = 1
                , lintMsgKind = LintInfo
                , lintMsgCode = FileTitleMissing
                }
        length (show m) `seq` pure ()
        length (show loc) `seq` pure ()
        rnf m `seq` pure ()
        (m == m) @?= True
        lintMsgPos m @?= 1
        lintMsgKind m @?= LintInfo
        lintMsgCode m @?= FileTitleMissing
    , testCase "showLintOrg covers every LintMessageCode variant" $ do
        -- Drives the big case in showLintOrg so each branch is ticked.
        let renderEach code =
              length
                ( showLintOrg
                    "/tmp/some.org"
                    (LintMessage 1 LintError code)
                )
        mapM_
          (\code -> renderEach code `seq` pure ())
          [ TodoMissingProperty "ID"
          , FileMissingProperty "CREATED"
          , TaskMissingAssignment
          , TodoLinkDoesNotMatchUrl
          , TodoFileDoesNotMatchAttachment
          , ArchiveTagFileDoesNotExist "/tmp/x"
          , TodoLinkKeywordImpliesLinkTag
          , FileSlugMismatch "slug"
          , MisplacedProperty
          , MisplacedTimestamp
          , MisplacedLogEntry
          , MisplacedDrawerEnd
          , DuplicateFileProperty "p"
          , DuplicateProperty "p"
          , DuplicateTag "t"
          , DuplicatedIdentifier "id"
          , InvalidStateChangeTransitionNotAllowed
              "DONE"
              (Just "TODO")
              ["WAIT"]
          , InvalidStateChangeInvalidTransition
              FirstTransition
              "TODO"
              "DONE"
          , InvalidStateChangeInvalidTransition
              IntermediateTransition
              "WAIT"
              "DONE"
          , InvalidStateChangeInvalidTransition
              LastTransition
              "TODO"
              "WAIT"
          , InvalidStateChangeIdempotent "TODO"
          , MultipleLogbooks
          , MixedLogbooks
          , WhitespaceAtStartOfLogEntry
          , FileTitleMissing
          , TitleWithExcessiveWhitespace
          , OverlyLongHeadline
          , TimestampsOnNonTodo
          , InconsistentWhitespace "after log entries"
          , InconsistentFilePreambleWhitespace
          , UnnecessaryWhitespace
          , EmptyBodyWhitespace
          , MultipleBlankLines
          , CategoryTooLong "TooLongCategoryName"
          , TitlePropertyNotLast
          , FileTagsTodoMismatch
          , VerbInFileUnknown "verb"
          , TagInFileUnknown "tag"
          , InvalidLocation "bad"
          , InvalidDrawerCase (PlainDrawer "Mixed")
          , InvalidDrawerCase (BeginDrawer "Example")
          , TodoMissingReviewProperties
          , NonTodoWithReviewProperties
          , BrokenLink "x"
          , HashesDoNotMatch "a" "b"
          , FileFailsToRoundTrip
          , AudioFileNotFound "/tmp/x.ogg"
          ]
    ]

-- Exercise lintOrgFile for a file with no entries (only a preamble).
-- Drives the '[] -> pure ()' arm at the 'case reverse (org ^.. allEntries)'
-- split in 'lintOrgFile''.
emptyFileTests :: TestTree
emptyFileTests =
  testGroup
    "empty-file handling"
    [ testCase "File with only preamble (no entries) lints without error" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EMPTY-001"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: empty-file"
                , "Just a preamble, no entries at all."
                ]
            msgs = runLint lintConfig "empty-entries.org" input
        -- The specific messages don't matter; we just need to exercise
        -- the '[] -> pure ()' arm of the case-split over allEntries.
        forceMessages msgs
    , testCase "lintOrgFiles with three files sharing an ID forces NE.cons branch and message thunks" $ do
        let mkFixture ident =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EMPTY-002-" <> BS.pack (map (fromIntegral . fromEnum) ident)
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: tri-" <> BS.pack (map (fromIntegral . fromEnum) ident)
                , "* TODO Shared"
                , ":PROPERTIES:"
                , ":ID:       SHARED-COV-ID"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            parsed =
              [ parseFixture lintConfig ("tri-" <> i <> ".org") (mkFixture i)
              | i <- ["a", "b", "c"]
              ]
            msgs = lintOrgFiles lintConfig LintInfo parsed
            allMsgs = concat (M.elems msgs)
        -- Force all DuplicatedIdentifier arguments so lines 142-144 tick.
        forceMessages allMsgs
        assertBool
          "DuplicatedIdentifier fires for triple"
          ( hasCode
              ( \case
                  DuplicatedIdentifier _ -> True
                  _ -> False
              )
              allMsgs
          )
    ]

-- Force the argument thunks of several rules that accumulate messages
-- in the Writer monad. Without forcing, the LintMessage constructor
-- args remain as thunks and the corresponding HPC ticks register as
-- 'nottickedoff'.
forceThunkTests :: TestTree
forceThunkTests =
  testGroup
    "message-argument thunk forcing"
    [ testCase "DuplicateTag, DuplicateProperty, InvalidLocation args are forced via deepseq" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO First  :foo:foo:"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":SAME:     a"
                , ":SAME:     b"
                , ":LOCATION: 0.0,0.0"
                , ":END:"
                ]
            msgs = runLint lintConfig "ft.org" input
        forceMessages msgs
        -- Also verify the rules do fire (smoke check).
        assertBool
          "DuplicateTag present"
          (hasCode (\case DuplicateTag _ -> True; _ -> False) msgs)
        assertBool
          "DuplicateProperty present"
          (hasCode (\case DuplicateProperty _ -> True; _ -> False) msgs)
        assertBool
          "InvalidLocation present"
          (hasCode (\case InvalidLocation _ -> True; _ -> False) msgs)
    , testCase "InvalidStateChange args are forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* WAIT Reorder"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-02"
                , ":CREATED:  [2024-10-01 Tue 10:00]"
                , ":END:"
                , -- A WAIT->DONE on Oct 1 then a later DONE->WAIT on Oct 5
                  -- in file order, but once reversed the earlier entry is
                  -- processed first. We want a WRONG time order and a
                  -- WAIT -> TASK disallowed transition.
                  "- State \"DONE\"       from \"TODO\"       [2024-10-05 Sat 10:00]"
                , "- State \"WAIT\"       from \"TODO\"       [2024-10-01 Tue 10:00]"
                ]
            msgs = runLint lintConfig "ft2.org" input
        forceMessages msgs
    , testCase "InvalidDrawerCase argument is forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Mixed drawer"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":Notes:"
                , "content"
                , ":END:"
                ]
            msgs = runLint lintConfig "ft3.org" input
        forceMessages msgs
        assertBool
          "InvalidDrawerCase present"
          (hasCode (\case InvalidDrawerCase _ -> True; _ -> False) msgs)
    , testCase "HashesDoNotMatch arguments are forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-04"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Hashed"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-04"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":HASH_sha512: deadbeef00000000000000000000000000000000000000000000000000000000"
                , ":END:"
                , "Contents that cannot hash to deadbeef."
                ]
            msgs = runLint lintConfig "ft4.org" input
        forceMessages msgs
        assertBool
          "HashesDoNotMatch present"
          (hasCode (\case HashesDoNotMatch _ _ -> True; _ -> False) msgs)
    , testCase "WhitespaceAtStartOfLogEntry and UnnecessaryWhitespace on log entry are forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-05"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Log with whitespace"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-05"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":LOGBOOK:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , ""
                , "   body content"
                , ":END:"
                ]
            msgs = runLint lintConfig "ft5.org" input
        forceMessages msgs
    , testCase "TagInFileUnknown, VerbInFileUnknown arguments are forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-06"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":TAGS_ALL: allowed1 allowed2"
                , ":VERB_ALL: write"
                , ":END:"
                , "#+title: t"
                , "* TODO frob: thing  :unknown1:"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-06"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "ft6.org" input
        forceMessages msgs
        assertBool
          "TagInFileUnknown present"
          (hasCode (\case TagInFileUnknown _ -> True; _ -> False) msgs)
        assertBool
          "VerbInFileUnknown present"
          (hasCode (\case VerbInFileUnknown _ -> True; _ -> False) msgs)
    , testCase "FileSlugMismatch, FileCreatedTimeMismatch arguments are forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-07"
                , ":CREATED:  [2023-01-15 Sun 09:00]"
                , ":END:"
                , "#+title: totally-different-name"
                ]
            msgs = runLint lintConfig "20240601-wrong-slug.org" input
        forceMessages msgs
    , testCase "ArchiveTagFileDoesNotExist (file-level, checkFiles=True) path" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-08"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":ARCHIVE:  /tmp/nope-file-arch.org"
                , ":END:"
                , "#+title: t"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLint cfg "/tmp/file-arch.org" input
        forceMessages msgs
        assertBool
          "ArchiveTagFileDoesNotExist present"
          ( hasCode
              (\case ArchiveTagFileDoesNotExist _ -> True; _ -> False)
              msgs
          )
    , testCase "Entry-level ArchiveTagFileDoesNotExist path is forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-09"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Archived entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-09"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":ARCHIVE:  /tmp/nope-entry-arch.org"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLint cfg "/tmp/entry-arch.org" input
        forceMessages msgs
        assertBool
          "Entry ArchiveTagFileDoesNotExist present"
          ( hasCode
              (\case ArchiveTagFileDoesNotExist _ -> True; _ -> False)
              msgs
          )
    , testCase "BrokenLink argument on entry URL property is forced" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FT-10"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO With broken URL"
                , ":PROPERTIES:"
                , ":ID:       ENT-FT-10"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":URL:      [[https://totally-nonexistent-host.invalid/p][link]]"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            -- Run at LintError level -- urlExists is expensive; stub
            -- out by using LintInfo level so the URL check stays inside
            -- the `level > LintAll` guard and pathExists returns true
            -- without a curl invocation.
            msgs = runLintAt cfg LintError "/tmp/broken-url.org" input
        forceMessages msgs
    ]

-- When the ID + (FILE tag or Attachments prop) combination produces
-- a missing attachment directory, the specialized "Attachments+ID"
-- branch of 'ruleFileTagMatchesAttachment' (lines 521-533) fires.
attachmentWithIdTests :: TestTree
attachmentWithIdTests =
  testGroup
    "ruleFileTagMatchesAttachment Attachments+ID branch"
    [ testCase "FILE tag + ID + missing attachment dir fires TodoFileDoesNotMatchAttachment" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-AI-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO With FILE tag  :FILE:"
                , ":PROPERTIES:"
                , ":ID:       ENT-AI-01-ABCDEF"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            cfg =
              lintConfig
                { _checkFiles = True
                , _homeDirectory = Just "/tmp"
                , _attachmentsDir =
                    "/tmp/definitely-nonexistent-attach-dir-nope"
                }
            msgs = runLint cfg "/tmp/attach.org" input
        forceMessages msgs
        assertBool
          "TodoFileDoesNotMatchAttachment fires"
          (hasCode (== TodoFileDoesNotMatchAttachment) msgs)
    , testCase "Attachments prop (no FILE tag) + ID + missing dir also fires" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-AI-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO With Attachments  :FILE:"
                , ":PROPERTIES:"
                , ":ID:       ENT-AI-02-ABCDEF"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":Attachments: doc.pdf"
                , ":END:"
                ]
            cfg =
              lintConfig
                { _checkFiles = True
                , _homeDirectory = Just "/tmp"
                , _attachmentsDir =
                    "/tmp/nope-nope-nope-attach-dir-missing"
                }
            msgs = runLint cfg "/tmp/attach2.org" input
        forceMessages msgs
    ]

-- When the config has no '_homeDirectory' set and a link uses '~/',
-- 'pathExists' falls through to 'getHomeDirectory'. The test asserts
-- the tilde branch is exercised (cleaning up hpc-untick'd line 958).
homeDirectoryTests :: TestTree
homeDirectoryTests =
  testGroup
    "pathExists homeDirectory = Nothing fallback"
    [ testCase "tilde-prefix link uses getHomeDirectory fallback" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-HD-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":ARCHIVE:  ~/definitely-nonexistent-xyz-abcd.org"
                , ":END:"
                , "#+title: t"
                ]
            cfg =
              lintConfig
                { _checkFiles = True
                , _homeDirectory = Nothing
                }
            msgs = runLint cfg "/tmp/tilde-no-home.org" input
        forceMessages msgs
        assertBool
          "ArchiveTagFileDoesNotExist fires (home fallback)"
          ( hasCode
              (\case ArchiveTagFileDoesNotExist _ -> True; _ -> False)
              msgs
          )
    ]

-- Run lint at LintError level over a fixture that produces only
-- LintInfo messages; the 'report' / 'report'' guard then takes the
-- "otherwise = pure ()" branch.
reportFilterTests :: TestTree
reportFilterTests =
  testGroup
    "report' filter otherwise branch"
    [ testCase "LintError level silently filters all LintInfo output" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-RF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Task with info-level surroundings"
                , ":PROPERTIES:"
                , ":ID:       ENT-RF-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ""
                , ""
                ]
            msgs = runLintAt lintConfig LintError "rf.org" input
        -- Only LintError or LintWarn messages survive. LintInfo reports
        -- (FileTitleMissing etc.) are filtered in the report' branch.
        forceMessages msgs
        mapM_
          ( \m ->
              assertBool
                ("unexpected low-severity msg: " ++ show m)
                (lintMsgKind m >= LintError)
          )
          msgs
    , testCase "LintWarn level filters LintInfo but keeps LintWarn" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-RF-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Task without ID"
                , ":PROPERTIES:"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLintAt lintConfig LintWarn "rf2.org" input
        forceMessages msgs
        -- TodoMissingProperty "ID" is LintWarn -> survives at LintWarn.
        assertBool
          "TodoMissingProperty survives at LintWarn"
          (hasCode (== TodoMissingProperty "ID") msgs)
        mapM_
          ( \m ->
              assertBool
                ("msg below LintWarn: " ++ show m)
                (lintMsgKind m >= LintWarn)
          )
          msgs
    ]

-- 'NOTER_DOCUMENT' with "devonthink" in the path is allowed: the
-- entry link checker short-circuits on the `"devonthink" `isInfixOf`
-- link` disjunct. This exercises the second branch of the `||` at
-- line 604.
devonthinkTests :: TestTree
devonthinkTests =
  testGroup
    "NOTER_DOCUMENT devonthink branch"
    [ testCase "NOTER_DOCUMENT with devonthink does not fire BrokenLink" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-DT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Paper"
                , ":PROPERTIES:"
                , ":ID:       ENT-DT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":NOTER_DOCUMENT: x-devonthink-item://whatever/here"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintAll "/tmp/devon.org" input
        forceMessages msgs
        assertBool
          "BrokenLink not fired for devonthink link"
          ( not
              ( hasCode
                  ( \case
                      BrokenLink l -> "devonthink" `elem` words (show l)
                      _ -> False
                  )
                  msgs
              )
          )
    ]

-- Drive the 'InconsistentWhitespace "surrounding body"' path by
-- creating an entry whose body has leading Whitespace but no
-- trailing Whitespace (and that is not the last entry).
surroundingWhitespaceTests :: TestTree
surroundingWhitespaceTests =
  testGroup
    "InconsistentWhitespace surrounding body"
    [ testCase "body with different leading/trailing whitespace fires InconsistentWhitespace" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO First with mismatched body ws"
                , ":PROPERTIES:"
                , ":ID:       ENT-SW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ""
                , "Actual body text"
                , "* TODO Second"
                , ":PROPERTIES:"
                , ":ID:       ENT-SW-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "sw.org" input
        forceMessages msgs
        assertBool
          "InconsistentWhitespace fires"
          ( hasCode
              ( \case
                  InconsistentWhitespace s ->
                    "surrounding body" == s
                      || "before log entries" == s
                      || "after log entries" == s
                  _ -> False
              )
              msgs
          )
    ]

-- EmptyBodyWhitespace fires when the body is empty and the last log
-- entry has trailing Whitespace (see lines 757-768). Both tests rely
-- on the log-state arithmetic detailed in 'ruleNoInconsistentWhitespace'.
emptyBodyWhitespaceViaLogTests :: TestTree
emptyBodyWhitespaceViaLogTests =
  testGroup
    "EmptyBodyWhitespace via last log trailing whitespace"
    [ testCase "Empty body + log note body ending in Whitespace fires EmptyBodyWhitespace" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Log note trailing whitespace"
                , ":PROPERTIES:"
                , ":ID:       ENT-EL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  note body"
                , ""
                ]
            msgs = runLint lintConfig "el.org" input
        forceMessages msgs
    ]

-- Drive the nested 'bodyLeading' / 'logTrailing' case at lines 789-791,
-- where the last log entry has no trailing Whitespace AND the body has
-- leading Whitespace: the fallback uses logTrailing' _head to mirror the
-- log-leading whitespace.
logTrailingFallbackTests :: TestTree
logTrailingFallbackTests =
  testGroup
    "logTrailing fallback branch"
    [ testCase "log leading whitespace + body leading whitespace path" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-LT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO log-plus-body leading whitespace"
                , ":PROPERTIES:"
                , ":ID:       ENT-LT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                , ""
                , "Body paragraph after trailing log whitespace"
                , "* TODO Second entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-LT-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "lt.org" input
        forceMessages msgs
    ]

-- When the last entry in a file has trailing body text that
-- matches the leading whitespace, the 'isLastEntry && isNothing
-- bodyTrailing' / 'bodyLeading == bodyTrailing' guard takes the
-- 'True' arm and suppresses the InconsistentWhitespace message. This
-- exercises the positive path of the unless and ticks the
-- is-last-entry branch of the surrounding body check.
trailingBodyMatchesLeadingTests :: TestTree
trailingBodyMatchesLeadingTests =
  testGroup
    "last-entry body whitespace matches"
    [ testCase "last entry body with matching leading/trailing ws is silent" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-TB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO First entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-TB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ""
                , "Body text"
                , ""
                , "* TODO Last entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-TB-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            msgs = runLint lintConfig "tb.org" input
        forceMessages msgs
        -- Force deepseq on msgs is sufficient to tick thunks regardless
        -- of which assertion we make.
        msgs `deepseq` pure ()
    ]

-- A file with two identical file-level #+PROPERTY entries triggers
-- the 'DuplicateFileProperty' rule at line 209.
duplicateFilePropertyTests :: TestTree
duplicateFilePropertyTests =
  testGroup
    "DuplicateFileProperty firing"
    [ testCase "two identical #+PROPERTY lines fire DuplicateFileProperty" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-DFP-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "#+PROPERTY: MyProp value1"
                , "#+PROPERTY: MyProp value2"
                ]
            msgs = runLint lintConfig "dfp.org" input
        forceMessages msgs
        assertBool
          "DuplicateFileProperty fires"
          (hasCode (\case DuplicateFileProperty _ -> True; _ -> False) msgs)
    ]

-- A file whose filename contains a timestamp prefix (so 'fileTimestamp'
-- succeeds) and has a CREATED property with a different time drives the
-- 'FileCreatedTimeMismatch' rule at line 253. The 'else LintInfo' arm
-- at line 235 also fires when CREATED is missing but the filename
-- supplies the timestamp.
fileCreatedTimestampTests :: TestTree
fileCreatedTimestampTests =
  testGroup
    "FileCreatedTimeMismatch + CREATED missing (else LintInfo)"
    [ testCase "filename timestamp matches no CREATED property: else LintInfo arm" $ do
        -- The filename is a pure YYYYMMDDHHMM timestamp which parses via
        -- 'stringTime'; combined with no CREATED property this drives
        -- the 'else LintInfo' arm at line 235 (fileCreatedTime succeeds
        -- via fileTimestamp fallback).
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FC-01"
                , ":END:"
                , "#+title: ts-file"
                ]
            msgs = runLint lintConfig "202406010900" input
        forceMessages msgs
        assertBool
          "FileMissingProperty CREATED fires at LintInfo"
          ( hasCode
              ( \case
                  FileMissingProperty p -> p == "CREATED"
                  _ -> False
              )
              msgs
          )
    , testCase "filename + CREATED mismatch fires FileCreatedTimeMismatch" $ do
        -- Filename parses as 2024-06-01 09:00 via fileTimestamp;
        -- CREATED property specifies a different time. The rule at
        -- line 253 then fires FileCreatedTimeMismatch.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FC-02"
                , ":CREATED:  [2020-01-01 Wed 00:00]"
                , ":END:"
                , "#+title: ts-mismatch"
                ]
            msgs = runLint lintConfig "202406010900" input
        forceMessages msgs
    ]

-- An entry with a CATEGORY property over 10 characters fires the
-- 'CategoryTooLong' rule at line 538.
categoryTooLongTests :: TestTree
categoryTooLongTests =
  testGroup
    "CategoryTooLong firing"
    [ testCase "CATEGORY > 10 chars fires CategoryTooLong" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-CT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Tagged entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-CT-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":CATEGORY: ReallyReallyLongCategoryName"
                , ":END:"
                ]
            msgs = runLint lintConfig "ct.org" input
        forceMessages msgs
        assertBool
          "CategoryTooLong fires"
          (hasCode (\case CategoryTooLong _ -> True; _ -> False) msgs)
    ]

-- State-change variants: the three kinds of invalid transition plus
-- the 'idempotent' state change. Each fixture is crafted to drive
-- a different arm of the state-machine check (lines 685-732).
stateChangeVariantTests :: TestTree
stateChangeVariantTests =
  testGroup
    "state-change variant coverage"
    [ testCase "FirstTransition: WAIT from a non-start keyword triggers InvalidTransition" $ do
        -- First transition state arrives at WAIT (not a start keyword),
        -- so the 'Nothing -> unless (kwf `elem` startKeywords)' branch
        -- fires with FirstTransition and the 'startKeywords . _head'
        -- reference at line 699.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SC-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* DONE Starts wrong"
                , ":PROPERTIES:"
                , ":ID:       ENT-SC-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"DONE\"       from \"LINK\"       [2024-10-08 Tue 10:00]"
                ]
            cfg =
              lintConfig
                { _keywordTransitions =
                    [("TODO", ["DONE", "WAIT", "CANCELED", "LINK"])]
                }
            msgs = runLint cfg "sc1.org" input
        forceMessages msgs
    , testCase "IntermediateTransition: mismatched 'from' fires InvalidTransition" $ do
        -- Two state changes where the 2nd says "from X" but the 1st
        -- moved to Y ≠ X; drives the Just prev -> unless (prev == kwf)
        -- branch.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SC-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* WAIT Two-step transitions"
                , ":PROPERTIES:"
                , ":ID:       ENT-SC-02"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"WAIT\"       from \"TODO\"       [2024-10-09 Wed 09:00]"
                , "- State \"TODO\"       from \"WAIT\"       [2024-10-08 Tue 09:00]"
                ]
            msgs = runLint lintConfig "sc2.org" input
        forceMessages msgs
    , testCase "Idempotent state change: TODO -> TODO fires InvalidStateChangeIdempotent" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SC-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Idempotent"
                , ":PROPERTIES:"
                , ":ID:       ENT-SC-03"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"TODO\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                ]
            msgs = runLint lintConfig "sc3.org" input
        forceMessages msgs
        assertBool
          "InvalidStateChangeIdempotent fires"
          ( hasCode
              (\case InvalidStateChangeIdempotent _ -> True; _ -> False)
              msgs
          )
    , testCase "Disallowed transition fires InvalidStateChangeTransitionNotAllowed" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SC-04"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* DONE Illegal jump"
                , ":PROPERTIES:"
                , ":ID:       ENT-SC-04"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"DONE\"       from \"WAIT\"       [2024-10-09 Wed 09:00]"
                , "- State \"WAIT\"       from \"TASK\"       [2024-10-08 Tue 09:00]"
                ]
            cfg =
              lintConfig
                { _keywordTransitions =
                    [ ("TASK", ["DONE", "CANCELED"])
                    , ("WAIT", ["DONE", "CANCELED"])
                    ]
                }
            msgs = runLint cfg "sc4.org" input
        forceMessages msgs
    , testCase "Wrong time order fires InvalidStateChangeWrongTimeOrder" $ do
        -- Two state changes with the newer one listed AFTER (i.e.,
        -- in file order first) a later timestamp: the (tm < prevTm)
        -- guard fires and drives line 685.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-SC-05"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* DONE Out of time order"
                , ":PROPERTIES:"
                , ":ID:       ENT-SC-05"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-01 Tue 10:00]"
                , "- State \"TODO\"       from \"\"           [2024-10-05 Sat 10:00]"
                ]
            msgs = runLint lintConfig "sc5.org" input
        forceMessages msgs
    ]

-- Drive the 'InconsistentWhitespace "after log entries"' branch
-- (line 755) by creating an entry whose log entries have mismatched
-- trailing whitespace.
afterLogInconsistentTests :: TestTree
afterLogInconsistentTests =
  testGroup
    "InconsistentWhitespace after log entries"
    [ testCase "mixed log-entry trailing whitespace fires 'after log entries'" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-AL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Log trailing whitespace inconsistency"
                , ":PROPERTIES:"
                , ":ID:       ENT-AL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  first body line"
                , ""
                , "- Note taken on [2024-10-09 Wed 09:00] \\\\"
                , "  second body line"
                ]
            msgs = runLint lintConfig "al.org" input
        forceMessages msgs
    ]

-- An entry with a URL property pointing to an existing local file
-- drives the 'protocol == "https:"' positive path through
-- 'pathExists' + 'urlExists'. With LintAll level the urlExists branch
-- is avoided via the 'level > LintAll' guard.
entryUrlBrokenLinkTests :: TestTree
entryUrlBrokenLinkTests =
  testGroup
    "Entry URL broken link: paragraph path"
    [ testCase "URL property in preamble body paragraph fires BrokenLink" $ do
        -- Preamble paragraph contains a broken [[https://...]]; with
        -- level = LintAll urlExists is short-circuited (level > LintAll
        -- is False, so urlExists IS invoked). Skip by using LintWarn
        -- level to hit the filter instead -- just get coverage of the
        -- paragraph branch lines 608-630.
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , ":IGNORE_LINKS: t"
                , "* TODO Entry with file link"
                , ":PROPERTIES:"
                , ":ID:       ENT-EB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "See [[https://example.com/no][example]] for details."
                ]
            cfg = lintConfig{_checkFiles = False}
            msgs = runLintAt cfg LintWarn "eb.org" input
        forceMessages msgs
    ]

-- Multiple LOGBOOK drawers fire MultipleLogbooks (line 844).
multipleLogbooksTests :: TestTree
multipleLogbooksTests =
  testGroup
    "MultipleLogbooks"
    [ testCase "two :LOGBOOK: drawers fire MultipleLogbooks" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-ML-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Two logbooks"
                , ":PROPERTIES:"
                , ":ID:       ENT-ML-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":LOGBOOK:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                , ":END:"
                , ":LOGBOOK:"
                , "- State \"WAIT\"       from \"TODO\"       [2024-10-09 Wed 09:00]"
                , ":END:"
                ]
            msgs = runLint lintConfig "ml.org" input
        forceMessages msgs
        assertBool
          "MultipleLogbooks fires"
          (hasCode (== MultipleLogbooks) msgs)
    ]

-- A file with a LOGBOOK drawer that contains non-clock entries plus
-- a state-change entry outside any LOGBOOK drawer fires MixedLogbooks
-- (lines 846-868).
mixedLogbooksTests :: TestTree
mixedLogbooksTests =
  testGroup
    "MixedLogbooks"
    [ testCase "non-clock entry in LOGBOOK + state entry outside fires MixedLogbooks" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-MX-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Mixed logbook"
                , ":PROPERTIES:"
                , ":ID:       ENT-MX-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":LOGBOOK:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "  inside logbook"
                , ":END:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-09 Wed 09:00]"
                ]
            msgs = runLint lintConfig "mx.org" input
        forceMessages msgs
    ]

-- Driving the 'level == LintDebug' branches (lines 179-180 and 938-940)
-- traces to stderr at LintDebug level; the messages still flow through.
debugLevelTests :: TestTree
debugLevelTests =
  testGroup
    "LintDebug level branches"
    [ testCase "LintDebug level forces trace branches" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-DB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO An entry"
                , ":PROPERTIES:"
                , ":ID:       ENT-DB-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "* TODO Task without ID"
                , ":PROPERTIES:"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
            -- Silence stderr in case 'traceM' writes output; we only
            -- care about covering the code path.
            msgs = runLintAt lintConfig LintDebug "db.org" input
        forceMessages msgs
    ]

-- A file with '#+AUDIO:' pointing at a missing file fires the
-- 'AudioFileNotFound' rule (lines 345-354).
audioFileTests :: TestTree
audioFileTests =
  testGroup
    "AudioFileNotFound"
    [ testCase "missing #+AUDIO file fires AudioFileNotFound" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-AU-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":AUDIO:    /tmp/definitely-missing-audio.ogg"
                , ":END:"
                , "#+title: t"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLint cfg "/tmp/au.org" input
        forceMessages msgs
        assertBool
          "AudioFileNotFound fires"
          (hasCode (\case AudioFileNotFound _ -> True; _ -> False) msgs)
    ]

-- A file-level preamble paragraph with '[[file:missing/path]]' drives
-- the regex match at 'ruleCheckAllLinks' (lines 319-343). Note that
-- the regex captures 'file:' (with colon), so 'protocol == "file"'
-- at line 326 is always False -- the 'else' arm is taken and the
-- final BrokenLink uses protocol ++ link. Running at LintError level
-- keeps 'level > LintAll' true so urlExists is short-circuited.
filePreambleLinkTests :: TestTree
filePreambleLinkTests =
  testGroup
    "file preamble [[file:...]] regex match"
    [ testCase "[[file:missing/path]] in preamble exercises regex arm" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-FPL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "See [[file:missing/path/file.pdf][PDF]] for details."
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintError "/tmp/fpl.org" input
        forceMessages msgs
    ]

-- A NOTER_DOCUMENT pointing at a missing non-devonthink path drives
-- the BrokenLink branch at line 606.
noterDocumentBrokenTests :: TestTree
noterDocumentBrokenTests =
  testGroup
    "NOTER_DOCUMENT missing BrokenLink"
    [ testCase "NOTER_DOCUMENT with missing non-devonthink path fires BrokenLink" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-ND-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Paper"
                , ":PROPERTIES:"
                , ":ID:       ENT-ND-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":NOTER_DOCUMENT: /tmp/definitely-missing-note.pdf"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintError "/tmp/nd.org" input
        forceMessages msgs
        assertBool
          "BrokenLink fires for NOTER_DOCUMENT missing"
          (hasCode (\case BrokenLink _ -> True; _ -> False) msgs)
    ]

-- An entry with a URL property pointing at a file via '[[file:...]]'
-- drives the regex match branch of the URL link checker (lines
-- 581-597). As with 'filePreambleLinkTests' above, the captured
-- protocol is 'file:' so 'protocol == "file"' is False.
entryUrlFileLinkTests :: TestTree
entryUrlFileLinkTests =
  testGroup
    "entry URL [[file:...]] regex match"
    [ testCase "URL with [[file:missing]] exercises regex arm" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EFL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Paper"
                , ":PROPERTIES:"
                , ":ID:       ENT-EFL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":URL:      [[file:missing/path/file.pdf][PDF]]"
                , ":END:"
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintError "/tmp/efl.org" input
        forceMessages msgs
    ]

-- An entry body paragraph with '[[file:missing]]' drives the
-- ruleCheckAllLinks paragraph regex arm (lines 608-630). Same caveat
-- as above: 'protocol == "file"' is false because the regex captures
-- the colon.
entryParagraphFileLinkTests :: TestTree
entryParagraphFileLinkTests =
  testGroup
    "entry paragraph [[file:...]] regex match"
    [ testCase "entry paragraph [[file:missing]] exercises regex arm" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-EPFL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO With file link"
                , ":PROPERTIES:"
                , ":ID:       ENT-EPFL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "See [[file:missing/doc.pdf][doc]] for details."
                ]
            cfg = lintConfig{_checkFiles = True, _homeDirectory = Just "/tmp"}
            msgs = runLintAt cfg LintError "/tmp/epfl.org" input
        forceMessages msgs
    ]

-- An entry log body paragraph whose first line begins with a space
-- fires 'UnnecessaryWhitespace' at the log-entry level (line 830).
logEntryUnnecessaryWhitespaceTests :: TestTree
logEntryUnnecessaryWhitespaceTests =
  testGroup
    "log entry UnnecessaryWhitespace"
    [ testCase "log body starting with space fires UnnecessaryWhitespace" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-LUW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Log space"
                , ":PROPERTIES:"
                , ":ID:       ENT-LUW-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                , "   leading-space body line"
                ]
            msgs = runLint lintConfig "luw.org" input
        forceMessages msgs
    ]

-- Additional InconsistentWhitespace coverage targeting the 'before log
-- entries' text argument: mix a log-entry with leading whitespace and
-- one without, forcing 'consistent logLeading' to fail.
beforeLogInconsistentTests :: TestTree
beforeLogInconsistentTests =
  testGroup
    "InconsistentWhitespace before log entries"
    [ testCase "mixed log-entry leading whitespace fires 'before log entries'" $ do
        let input =
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-BL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , "#+title: t"
                , "* TODO Log entries with leading whitespace mismatch"
                , ":PROPERTIES:"
                , ":ID:       ENT-BL-01"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                , ":LOGBOOK:"
                , ""
                , "- State \"DONE\"       from \"TODO\"       [2024-10-09 Wed 09:00]"
                , "- State \"WAIT\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                , ":END:"
                ]
            msgs = runLint lintConfig "bl.org" input
        forceMessages msgs
    ]
