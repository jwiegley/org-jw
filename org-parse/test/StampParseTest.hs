{-# LANGUAGE OverloadedStrings #-}

module StampParseTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO"]
    , _closedKeywords = ["DONE"]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

parseEntries :: ByteString -> Either String [Entry]
parseEntries bs = case parseOrgFile parseConfig "t.org" bs of
  Right (OrgFile _ _ es) -> Right es
  Left (_, msg) -> Left msg

onlyEntry :: ByteString -> Either String Entry
onlyEntry bs = case parseEntries bs of
  Right [e] -> Right e
  Right _ -> Left "expected exactly one entry"
  Left msg -> Left msg

tests :: TestTree
tests =
  testGroup
    "parseStamp / parseStamps"
    [ testCase "CLOSED stamp" $ do
        e <- expectRight $ onlyEntry closedSingle
        case _entryStamps e of
          [ClosedStamp _ _] -> pure ()
          other -> assertFailure ("expected single ClosedStamp, got " ++ show other)
    , testCase "SCHEDULED stamp" $ do
        e <- expectRight $ onlyEntry scheduledSingle
        case _entryStamps e of
          [ScheduledStamp _ _] -> pure ()
          other -> assertFailure ("expected single ScheduledStamp, got " ++ show other)
    , testCase "DEADLINE stamp" $ do
        e <- expectRight $ onlyEntry deadlineSingle
        case _entryStamps e of
          [DeadlineStamp _ _] -> pure ()
          other -> assertFailure ("expected single DeadlineStamp, got " ++ show other)
    , testCase "multiple leading stamps separated by space (parseStamps)" $ do
        e <- expectRight $ onlyEntry multipleStamps
        length (_entryStamps e) @?= 2
    , testCase "all three leading stamps on one line" $ do
        e <- expectRight $ onlyEntry tripleStamps
        length (_entryStamps e) @?= 3
    , testCase "active timestamp on its own line yields ActiveStamp" $ do
        e <- expectRight $ onlyEntry activeTrailing
        case _entryStamps e of
          [ActiveStamp _ _] -> pure ()
          other -> assertFailure ("expected ActiveStamp, got " ++ show other)
    , testCase "leading stamps plus active stamp yield both" $ do
        e <- expectRight $ onlyEntry stampsAndActive
        length (_entryStamps e) @?= 2
        let kinds = map stampKind (_entryStamps e)
        kinds @?= ["Scheduled", "Active"]
    ]
 where
  stampKind (ClosedStamp _ _) = "Closed" :: String
  stampKind (ScheduledStamp _ _) = "Scheduled"
  stampKind (DeadlineStamp _ _) = "Deadline"
  stampKind (ActiveStamp _ _) = "Active"

expectRight :: Either String a -> IO a
expectRight = either assertFailure pure

closedSingle :: ByteString
closedSingle =
  fromLines
    [ "* DONE Done task"
    , "CLOSED: [2024-01-15 Mon 17:30]"
    ]

scheduledSingle :: ByteString
scheduledSingle =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon>"
    ]

deadlineSingle :: ByteString
deadlineSingle =
  fromLines
    [ "* TODO Task"
    , "DEADLINE: <2024-01-20 Sat>"
    ]

multipleStamps :: ByteString
multipleStamps =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon> DEADLINE: <2024-01-20 Sat>"
    ]

tripleStamps :: ByteString
tripleStamps =
  fromLines
    [ "* DONE Task"
    , "CLOSED: [2024-01-15 Mon] SCHEDULED: <2024-01-14 Sun> DEADLINE: <2024-01-20 Sat>"
    ]

activeTrailing :: ByteString
activeTrailing =
  fromLines
    [ "* TODO Task"
    , "<2024-01-15 Mon>"
    ]

stampsAndActive :: ByteString
stampsAndActive =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon>"
    , "<2024-02-01 Thu>"
    ]
