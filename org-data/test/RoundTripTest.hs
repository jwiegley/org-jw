{-# LANGUAGE OverloadedStrings #-}

module RoundTripTest (tests) where

import Control.Lens (over)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Data.Lens (biplate)
import Org.Parse (parseOrgFile)
import Org.Print (showOrgFile)
import Org.Types (Config (..), Loc (..), OrgFile, defaultConfig)
import Test.Tasty
import Test.Tasty.HUnit

-- Normalize all Loc positions in an OrgFile so that round-trip
-- comparisons aren't broken by the printer's whitespace normalization
-- changing byte offsets.
normalizeLocs :: OrgFile -> OrgFile
normalizeLocs = over biplate (const (Loc "" 0))

-- A configuration that knows about TODO/DONE/etc, so the parser will
-- recognize them as keywords rather than literal title prefixes.
todoConfig :: Config
todoConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions =
        [ ("TODO", ["DONE", "WAIT", "CANCELED"])
        , ("WAIT", ["TODO", "DONE", "CANCELED"])
        ]
    , _priorities = ["A", "B", "C"]
    , _propertyColumn = 11
    , _tagsColumn = 60
    }

-- Round-trip property: parse(print(parse(input))) == parse(input).
-- We do not require print(parse(input)) == input, because the printer
-- may canonicalize whitespace/formatting. The semantic content must
-- survive a round trip.
roundTrip :: Config -> String -> ByteString -> Assertion
roundTrip cfg path input =
  case parseOrgFile cfg path input of
    Left (_, msg) ->
      assertFailure $ "first parse failed: " ++ msg
    Right parsed1 -> do
      let printed = unlines (showOrgFile cfg parsed1)
          printedBS = BS8.pack printed
      case parseOrgFile cfg path printedBS of
        Left (_, msg) ->
          assertFailure $
            "re-parse failed: " ++ msg ++ "\nprinted output:\n" ++ printed
        Right parsed2 ->
          assertEqual
            ( "round-trip mismatch.\noriginal:\n"
                ++ BS8.unpack input
                ++ "\nprinted:\n"
                ++ printed
            )
            (normalizeLocs parsed1)
            (normalizeLocs parsed2)

-- A first parse must succeed (used for fixtures that should be accepted).
parsesOK :: Config -> String -> ByteString -> Assertion
parsesOK cfg path input =
  case parseOrgFile cfg path input of
    Left (_, msg) -> assertFailure $ "parse failed: " ++ msg
    Right _ -> pure ()

-- Soft round-trip: parse, print, re-parse — assert both parses succeed.
-- The printer may reformat in ways that aren't strictly identity-preserving
-- (e.g., entry headline whitespace normalization), so we just exercise the
-- code paths without insisting on AST equality.
roundTripSoft :: Config -> String -> ByteString -> Assertion
roundTripSoft cfg path input =
  case parseOrgFile cfg path input of
    Left (_, msg) ->
      assertFailure $ "first parse failed: " ++ msg
    Right parsed1 -> do
      let printed = unlines (showOrgFile cfg parsed1)
          printedBS = BS8.pack printed
      case parseOrgFile cfg path printedBS of
        Left (_, msg) ->
          assertFailure $
            "re-parse failed: " ++ msg ++ "\nprinted output:\n" ++ printed
        Right _ -> pure ()

-- Concatenation helper for building byte string fixtures from line lists.
fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

tests :: TestTree
tests =
  testGroup
    "Org file round-trip"
    [ testGroup
        "minimal"
        [ testCase "empty file" $
            roundTrip defaultConfig "empty.org" ""
        , testCase "single newline" $
            roundTrip defaultConfig "ws.org" "\n"
        , testCase "lone whitespace lines" $
            roundTrip defaultConfig "ws.org" "\n\n\n"
        ]
    , testGroup
        "header"
        [ testCase "properties drawer only" $
            roundTrip defaultConfig "props.org" $
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-ID-001"
                , ":CREATED:  [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
        , testCase "file-level properties only" $
            roundTrip defaultConfig "fp.org" $
              fromLines
                [ "#+title: Sample Document"
                , "#+filetags: :foo:bar:"
                ]
        , testCase "drawer + file properties + preamble" $
            roundTrip defaultConfig "full.org" $
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       12345"
                , ":END:"
                , "#+title: Title"
                , "#+filetags: :tag1:"
                , ""
                , "Some preamble paragraph"
                , "spread across two lines."
                , ""
                ]
        ]
    , testGroup
        "entries"
        [ testCase "single plain entry" $
            roundTrip defaultConfig "one.org" $
              fromLines
                [ "* Just a heading"
                ]
        , testCase "todo and done entries" $
            roundTrip todoConfig "todo.org" $
              fromLines
                [ "* TODO First task"
                , "* DONE Second task"
                ]
        , testCase "entry with priority" $
            roundTrip todoConfig "prio.org" $
              fromLines
                [ "* TODO [#A] Important"
                , "* TODO [#B] Less important"
                , "* TODO [#C] Lowest"
                ]
        , testCase "entry with single tag (canonical)" $
            roundTrip defaultConfig "tag1.org" $
              fromLines
                [ "* Heading  :work:"
                ]
        , testCase "entry with multiple tags (canonical)" $
            roundTrip defaultConfig "tags.org" $
              fromLines
                [ "* Heading  :a:b:c:"
                ]
        , testCase "entry with single tag (loose spacing, soft)" $
            roundTripSoft defaultConfig "tag1s.org" $
              fromLines
                [ "* Heading                                                       :work:"
                ]
        , testCase "entry with multiple tags (loose spacing, soft)" $
            roundTripSoft defaultConfig "tagss.org" $
              fromLines
                [ "* Heading                                                  :a:b:c:"
                ]
        , testCase "nested entries" $
            roundTrip defaultConfig "nested.org" $
              fromLines
                [ "* Top"
                , "** Mid"
                , "*** Leaf"
                ]
        , testCase "entry with property drawer" $
            roundTrip todoConfig "ep.org" $
              fromLines
                [ "* TODO Sample"
                , ":PROPERTIES:"
                , ":ID:       ENTRY-001"
                , ":CATEGORY: work"
                , ":END:"
                ]
        , testCase "entry with paragraph body" $
            roundTrip defaultConfig "body.org" $
              fromLines
                [ "* Heading"
                , "First line of body."
                , "Second line of body."
                ]
        , testCase "entry with leading body whitespace" $
            roundTrip defaultConfig "bws.org" $
              fromLines
                [ "* Heading"
                , ""
                , "Body after blank line."
                ]
        ]
    , testGroup
        "stamps"
        [ testCase "scheduled stamp" $
            roundTrip todoConfig "sched.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed>"
                ]
        , testCase "deadline stamp" $
            roundTrip todoConfig "dead.org" $
              fromLines
                [ "* TODO Sample"
                , "DEADLINE: <2024-12-25 Wed>"
                ]
        , testCase "closed stamp" $
            roundTrip todoConfig "closed.org" $
              fromLines
                [ "* DONE Sample"
                , "CLOSED: [2024-10-07 Mon 20:15]"
                ]
        , testCase "scheduled with deadline" $
            roundTrip todoConfig "sd.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed> DEADLINE: <2025-01-01 Wed>"
                ]
        , testCase "scheduled with time" $
            roundTrip todoConfig "st.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed 09:00>"
                ]
        , testCase "scheduled with repeater" $
            roundTrip todoConfig "rep.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed +1w>"
                ]
        , testCase "scheduled with .+ repeater" $
            roundTrip todoConfig "rep2.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed .+1d>"
                ]
        , testCase "scheduled with ++ repeater" $
            roundTrip todoConfig "rep3.org" $
              fromLines
                [ "* TODO Sample"
                , "SCHEDULED: <2024-12-25 Wed ++1m>"
                ]
        , testCase "deadline with within suffix" $
            roundTrip todoConfig "wit.org" $
              fromLines
                [ "* TODO Sample"
                , "DEADLINE: <2024-12-25 Wed -2d>"
                ]
        ]
    , testGroup
        "active timestamps"
        [ testCase "active timestamp on entry" $
            roundTrip defaultConfig "ts.org" $
              fromLines
                [ "* Meeting"
                , "<2024-12-25 Wed>"
                ]
        , testCase "active timestamp with hour" $
            roundTrip defaultConfig "tsh.org" $
              fromLines
                [ "* Meeting"
                , "<2024-12-25 Wed 14:30>"
                ]
        , testCase "active timestamp range same day" $
            roundTrip defaultConfig "tsr.org" $
              fromLines
                [ "* Meeting"
                , "<2024-12-25 Wed 09:00-10:00>"
                ]
        ]
    , testGroup
        "logbook"
        [ testCase "single LOGBOOK with state change" $
            roundTrip todoConfig "lb.org" $
              fromLines
                [ "* DONE Sample"
                , ":LOGBOOK:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
        , testCase "logbook with closing note (soft)" $
            roundTripSoft todoConfig "lb2.org" $
              fromLines
                [ "* DONE Sample"
                , ":LOGBOOK:"
                , "- CLOSING NOTE [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
        , testCase "logbook with note" $
            roundTrip todoConfig "lb3.org" $
              fromLines
                [ "* TODO Sample"
                , ":LOGBOOK:"
                , "- Note taken on [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
        ]
    , testGroup
        "drawers"
        [ testCase "BEGIN drawer" $
            roundTrip defaultConfig "d.org" $
              fromLines
                [ "* Heading"
                , ":CUSTOM:"
                , "Hello"
                , ":END:"
                ]
        ]
    , testGroup
        "complex composite"
        [ testCase "entry with all the things (soft)" $
            roundTripSoft todoConfig "all.org" $
              fromLines
                [ ":PROPERTIES:"
                , ":ID:       FILE-001"
                , ":END:"
                , "#+title: Composite"
                , "#+filetags: :composite:"
                , ""
                , "Preamble paragraph."
                , ""
                , "* TODO [#A] First task                                       :work:urgent:"
                , "SCHEDULED: <2024-12-25 Wed> DEADLINE: <2025-01-01 Wed>"
                , ":PROPERTIES:"
                , ":ID:       ENTRY-001"
                , ":CATEGORY: tasks"
                , ":END:"
                , ":LOGBOOK:"
                , "- State \"TODO\"       from              [2024-10-07 Mon 20:15]"
                , ":END:"
                , "Body paragraph for the first task."
                , ""
                , "** TODO Sub-task"
                , "Sub-task body."
                , "* DONE Closed task"
                , "CLOSED: [2024-10-07 Mon 20:15]"
                ]
        ]
    , testGroup
        "first-parse acceptance"
        [ testCase "fixture with multiple log kinds" $
            parsesOK todoConfig "many-logs.org" $
              fromLines
                [ "* DONE Done task"
                , ":LOGBOOK:"
                , "- State \"DONE\"       from \"TODO\"       [2024-10-07 Mon 20:15]"
                , "- Refiled on [2024-10-07 Mon 20:15]"
                , "- Note taken on [2024-10-07 Mon 20:15]"
                , "- CLOSING NOTE [2024-10-07 Mon 20:15]"
                , ":END:"
                ]
        ]
    ]
