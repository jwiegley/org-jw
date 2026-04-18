{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Additional lint-rule tests targeting rules and branches that the
'LintRulesTest' suite does not exercise. These are written to raise
HPC expression coverage on 'Org.Lint' by driving code paths that
were previously unreached: entry-level ARCHIVE/URL/NOTER_DOCUMENT
link checks, HASH_sha512 mismatch, BeginDrawer casing, the
FirstTransition and IntermediateTransition state-change paths, the
"Attending" family of meeting titles for EmptyBodyWhitespace, the
LogBody leading-whitespace path for UnnecessaryWhitespace, the
pathExists "~/" branch, and the NE.cons branch of lintOrgFiles.
-}
module LintCheckersTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Org.Lint
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- A minimal configuration matching the one in 'LintRulesTest'. We
-- duplicate it here so the module is self-contained and does not
-- depend on 'LintRulesTest' internals.
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

parseFixture :: Config -> (FilePath, ByteString) -> OrgFile
parseFixture cfg (path, bs) =
  case parseOrgFile cfg path bs of
    Left (_, msg) ->
      error $ "test fixture must parse: " ++ msg ++ "\ninput: " ++ show bs
    Right org -> org

hasCode :: (LintMessageCode -> Bool) -> [LintMessage] -> Bool
hasCode p = any (p . lintMsgCode)

shouldFire ::
  String ->
  (LintMessageCode -> Bool) ->
  [LintMessage] ->
  Assertion
shouldFire desc p msgs =
  assertBool
    ("expected " ++ desc ++ " in " ++ show (map lintMsgCode msgs))
    (hasCode p msgs)

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

tests :: TestTree
tests =
  testGroup
    "Additional lint checker paths"
    [ testGroup
        "entry-level ruleArchiveTagFileExists"
        [ testCase "ArchiveTagFileDoesNotExist fires for entry ARCHIVE with checkFiles" $
            shouldFire
              "entry ArchiveTagFileDoesNotExist"
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
                  "/tmp/entry-arch.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-01"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Archived entry"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-01"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":ARCHIVE:  /tmp/definitely-nonexistent-archive.org"
                      , ":END:"
                      ]
                  )
              )
        ]
    , testGroup
        "entry-level ruleCheckAllLinks"
        [ testCase "BrokenLink fires for entry URL property with missing file link" $
            shouldFire
              "BrokenLink (entry URL)"
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
                  "/tmp/entry-url.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-02"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Has URL property"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-02"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":URL:      [[file:/tmp/definitely-nonexistent/nope.org][here]]"
                      , ":END:"
                      ]
                  )
              )
        , testCase "BrokenLink fires for entry NOTER_DOCUMENT missing file" $
            shouldFire
              "BrokenLink (NOTER_DOCUMENT)"
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
                  "/tmp/entry-noter.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-03"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Has NOTER document"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-03"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":NOTER_DOCUMENT: /tmp/definitely-nonexistent/doc.pdf"
                      , ":END:"
                      ]
                  )
              )
        , testCase "BrokenLink fires for entry-body paragraph with missing file" $
            shouldFire
              "BrokenLink (entry paragraph)"
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
                  "/tmp/entry-body-link.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-04"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Entry with inline link"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-04"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "See [[file:/tmp/definitely-nonexistent/ref.org][ref]]."
                      ]
                  )
              )
        ]
    , testGroup
        "InvalidStateChangeInvalidTransition FirstTransition"
        [ testCase "FirstTransition fires when from-state is not a start keyword" $
            shouldFire
              "FirstTransition"
              ( \case
                  InvalidStateChangeInvalidTransition FirstTransition _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "first-trans.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-05"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Bad first transition"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-05"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , -- 'DONE' is not in startKeywords, so a log history
                        -- that begins at DONE will fire FirstTransition.
                        "- State \"TODO\"       from \"DONE\"       [2024-10-08 Tue 09:00]"
                      ]
                  )
              )
        ]
    , testGroup
        "InvalidStateChangeInvalidTransition IntermediateTransition"
        [ testCase "IntermediateTransition fires when adjacent log entries mismatch" $
            shouldFire
              "IntermediateTransition"
              ( \case
                  InvalidStateChangeInvalidTransition IntermediateTransition _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "inter-trans.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-06"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* DONE Chain mismatch"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-06"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , -- The log is in reverse-chronological order in the
                        -- file (most recent first). Processed by foldM in
                        -- reverse (oldest first), WAIT->DONE is seen first
                        -- (sets prev=DONE), then the second log entry's
                        -- from-state is TODO, which doesn't match prev=DONE
                        -- -> fires IntermediateTransition.
                        "- State \"DONE\"       from \"TODO\"       [2024-10-09 Wed 09:00]"
                      , "- State \"WAIT\"       from \"TODO\"       [2024-10-08 Tue 09:00]"
                      ]
                  )
              )
        ]
    , testGroup
        "ruleHashesMatch"
        [ testCase "HashesDoNotMatch fires for mismatched HASH_sha512 property" $
            shouldFire
              "HashesDoNotMatch"
              ( \case
                  HashesDoNotMatch _ _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "hash.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-07"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Hashed entry"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-07"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":HASH_sha512: 0000000000000000000000000000000000000000000000000000000000000000"
                      , ":END:"
                      , "Some body content that definitely does not hash to zeros."
                      ]
                  )
              )
        ]
    , testGroup
        "ruleDrawerCase BeginDrawer"
        [ testCase "InvalidDrawerCase fires for uppercase #+BEGIN block" $
            shouldFire
              "InvalidDrawerCase (BeginDrawer)"
              ( \case
                  InvalidDrawerCase _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "begin.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-08"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO Block entry"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-08"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+BEGIN_Example"
                      , "body"
                      , "#+END_Example"
                      ]
                  )
              )
        ]
    , testGroup
        "ruleNoEmptyBodyWhitespace special titles"
        [ testCase "EmptyBodyWhitespace fires for plain 'Attending' heading with whitespace body" $
            shouldFire
              "EmptyBodyWhitespace (Attending)"
              (== EmptyBodyWhitespace)
              ( runLint
                  lintConfig
                  "attending.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-09"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* Attending"
                      , ""
                      , ""
                      ]
                  )
              )
        , testCase "EmptyBodyWhitespace fires for plain 'Agenda' heading with whitespace body" $
            shouldFire
              "EmptyBodyWhitespace (Agenda)"
              (== EmptyBodyWhitespace)
              ( runLint
                  lintConfig
                  "agenda.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-10"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* Agenda"
                      , ""
                      , ""
                      ]
                  )
              )
        ]
    , testGroup
        "ruleNoUnnecessaryWhitespace LogBody"
        [ testCase "UnnecessaryWhitespace fires for logbook note body with leading space" $
            -- The rule traverses LogEntry children through 'uniplate'. Top-level
            -- log notes have no LogEntry children (uniplate on LogNote returns
            -- nothing), so we must wrap the note inside a LOGBOOK drawer. Then
            -- the note becomes a LogEntry child of LogBook that 'uniplate'
            -- reaches. The continuation indented by an extra space causes the
            -- parser to produce a Paragraph starting with ' ', hitting the
            -- LogBody arm of ruleNoUnnecessaryWhitespace.
            shouldFire
              "UnnecessaryWhitespace (log body)"
              (== UnnecessaryWhitespace)
              ( runLint
                  lintConfig
                  "uw-log.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-11"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO With log note leading ws"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-11"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ":LOGBOOK:"
                      , "- Note taken on [2024-10-08 Tue 09:00] \\\\"
                      , "   leading space on note body"
                      , ":END:"
                      ]
                  )
              )
        ]
    , testGroup
        "ignoreWhitespace file property"
        [ testCase "WHITESPACE: ignore suppresses InconsistentWhitespace" $
            shouldNotFire
              "InconsistentWhitespace (ignored)"
              ( \case
                  InconsistentWhitespace _ -> True
                  _ -> False
              )
              ( runLint
                  lintConfig
                  "ignore-ws.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-12"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":WHITESPACE: ignore"
                      , ":END:"
                      , "#+title: t"
                      , "* TODO First"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-12A"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      , ""
                      , "Some text"
                      , "* TODO Second"
                      , ":PROPERTIES:"
                      , ":ID:       ENTRY-CHK-12B"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":END:"
                      ]
                  )
              )
        ]
    , testGroup
        "pathExists tilde-prefix branch"
        [ testCase "pathExists exercises ~/ branch for missing ARCHIVE path" $
            -- Driving '~/...' through pathExists hits the tilde-prefix
            -- branch (homeDirectory is overridden to /tmp so the test is
            -- portable). We don't care whether the final doesFileExist
            -- call returns True or False for coverage — only that the
            -- tilde branch of pathExists is entered.
            shouldFire
              "ArchiveTagFileDoesNotExist (~/missing)"
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
                  "/tmp/tilde-archive-missing.org"
                  ( fromLines
                      [ ":PROPERTIES:"
                      , ":ID:       FILE-ID-CHK-14"
                      , ":CREATED:  [2024-10-07 Mon 20:15]"
                      , ":ARCHIVE:  ~/definitely-nonexistent-subdir/nope.org"
                      , ":END:"
                      , "#+title: t"
                      ]
                  )
              )
        ]
    , testGroup
        "lintOrgFiles DuplicatedIdentifier NE.cons branch"
        [ -- When three files share an ID, the 'NE.cons' branch of the
          -- 'entriesById' accumulator in 'lintOrgFiles' is forced. The
          -- first occurrence uses 'NE.singleton'; subsequent ones use
          -- 'NE.cons'.
          testCase "Three files sharing an ID still triggers DuplicatedIdentifier" $
            let inputs =
                  [
                    ( "dupe-a.org"
                    , fromLines
                        [ ":PROPERTIES:"
                        , ":ID:       FILE-ID-CHK-15A"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        , "#+title: a"
                        , "* TODO Shared triple"
                        , ":PROPERTIES:"
                        , ":ID:       TRIPLE-ID-01"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        ]
                    )
                  ,
                    ( "dupe-b.org"
                    , fromLines
                        [ ":PROPERTIES:"
                        , ":ID:       FILE-ID-CHK-15B"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        , "#+title: b"
                        , "* TODO Shared triple"
                        , ":PROPERTIES:"
                        , ":ID:       TRIPLE-ID-01"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        ]
                    )
                  ,
                    ( "dupe-c.org"
                    , fromLines
                        [ ":PROPERTIES:"
                        , ":ID:       FILE-ID-CHK-15C"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        , "#+title: c"
                        , "* TODO Shared triple"
                        , ":PROPERTIES:"
                        , ":ID:       TRIPLE-ID-01"
                        , ":CREATED:  [2024-10-07 Mon 20:15]"
                        , ":END:"
                        ]
                    )
                  ]
                parsed = map (parseFixture lintConfig) inputs
                msgs = lintOrgFiles lintConfig LintInfo parsed
                allMsgs = concat (M.elems msgs)
             in shouldFire
                  "DuplicatedIdentifier (triple)"
                  ( \case
                      DuplicatedIdentifier _ -> True
                      _ -> False
                  )
                  allMsgs
        ]
    ]
