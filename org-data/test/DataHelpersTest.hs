{-# LANGUAGE OverloadedStrings #-}

module DataHelpersTest (tests) where

import Control.Lens ((&), (.~), (^.))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

cfg :: Config
cfg =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions =
        [ ("TODO", ["DONE", "WAIT", "CANCELED"])
        , ("WAIT", ["TODO", "DONE"])
        ]
    }

archiveFile :: OrgFile
archiveFile =
  OrgFile
    { _orgFilePath = "/foo/archive/bar.org"
    , _orgFileHeader = Header [] [] (Body [])
    , _orgFileEntries = []
    }

nonArchiveFile :: OrgFile
nonArchiveFile = archiveFile{_orgFilePath = "/foo/bar.org"}

sampleLoc :: Loc
sampleLoc = Loc "sample.org" 0

tests :: TestTree
tests =
  testGroup
    "Org.Data helpers"
    [ testGroup
        "isTodo"
        [ testCase "open keyword is todo" $
            isTodo cfg "TODO" @?= True
        , testCase "closed keyword is todo" $
            isTodo cfg "DONE" @?= True
        , testCase "unknown keyword is not todo" $
            isTodo cfg "FOO" @?= False
        , testCase "empty string is not todo" $
            isTodo cfg "" @?= False
        , testCase "default config rejects everything" $
            isTodo defaultConfig "TODO" @?= False
        ]
    , testGroup
        "isOpenTodo"
        [ testCase "open keyword" $
            isOpenTodo cfg "TODO" @?= True
        , testCase "closed keyword is not open" $
            isOpenTodo cfg "DONE" @?= False
        , testCase "unknown keyword" $
            isOpenTodo cfg "XYZ" @?= False
        ]
    , testGroup
        "isArchive"
        [ testCase "path with archive substring" $
            isArchive archiveFile @?= True
        , testCase "path without archive substring" $
            isArchive nonArchiveFile @?= False
        , testCase "path equals archive" $
            isArchive (archiveFile{_orgFilePath = "archive"}) @?= True
        , testCase "path with archive as prefix" $
            isArchive (archiveFile{_orgFilePath = "archiveroot.org"}) @?= True
        ]
    , testGroup
        "transitionsOf"
        [ testCase "known keyword" $
            transitionsOf cfg "TODO" @?= ["DONE", "WAIT", "CANCELED"]
        , testCase "second known keyword" $
            transitionsOf cfg "WAIT" @?= ["TODO", "DONE"]
        , testCase "unknown keyword returns empty" $
            transitionsOf cfg "DONE" @?= []
        , testCase "default config returns empty" $
            transitionsOf defaultConfig "TODO" @?= []
        ]
    , testGroup
        "findDuplicates"
        [ testCase "no duplicates" $
            findDuplicates ([1, 2, 3] :: [Int]) @?= []
        , testCase "single duplicate" $
            findDuplicates ([1, 2, 2, 3] :: [Int]) @?= [2]
        , testCase "multiple duplicates sorted by key" $
            findDuplicates ([1, 1, 2, 2, 3] :: [Int]) @?= [1, 2]
        , testCase "three occurrences still counts once" $
            findDuplicates ([1, 1, 1] :: [Int]) @?= [1]
        , testCase "strings" $
            findDuplicates (["a", "b", "a", "c", "b"] :: [String]) @?= ["a", "b"]
        , testCase "empty" $
            findDuplicates ([] :: [Int]) @?= []
        ]
    , testGroup
        "hardCodedInheritedProperties"
        [ testCase "list is stable" $
            hardCodedInheritedProperties
              @?= ["COLUMNS", "CATEGORY", "ARCHIVE", "LOGGING"]
        , testCase "contains CATEGORY" $
            assertBool
              "CATEGORY present"
              ("CATEGORY" `elem` hardCodedInheritedProperties)
        ]
    , testGroup
        "TimestampFormat"
        [ testCase "tsFormatFmt HourMinSec" $
            tsFormatFmt HourMinSec @?= "%Y%m%d%H%M%S"
        , testCase "tsFormatFmt HourMin" $
            tsFormatFmt HourMin @?= "%Y%m%d%H%M"
        , testCase "tsFormatFmt JustDay" $
            tsFormatFmt JustDay @?= "%Y%m%d"
        , testCase "tsFormatLen HourMinSec" $
            tsFormatLen HourMinSec @?= 14
        , testCase "tsFormatLen HourMin" $
            tsFormatLen HourMin @?= 12
        , testCase "tsFormatLen JustDay" $
            tsFormatLen JustDay @?= 8
        ]
    , testGroup
        "keywordString lens"
        [ testCase "gets open keyword" $
            OpenKeyword sampleLoc "TODO" ^. keywordString @?= "TODO"
        , testCase "gets closed keyword" $
            ClosedKeyword sampleLoc "DONE" ^. keywordString @?= "DONE"
        , testCase "sets open keyword" $
            (OpenKeyword sampleLoc "TODO" & keywordString .~ "X")
              @?= OpenKeyword sampleLoc "X"
        , testCase "sets closed keyword" $
            (ClosedKeyword sampleLoc "DONE" & keywordString .~ "Z")
              @?= ClosedKeyword sampleLoc "Z"
        ]
    , testGroup
        "tagString lens"
        [ testCase "gets plain tag" $
            PlainTag "foo" ^. tagString @?= "foo"
        , testCase "sets plain tag" $
            (PlainTag "foo" & tagString .~ "bar") @?= PlainTag "bar"
        ]
    ]
