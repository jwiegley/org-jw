{-# LANGUAGE OverloadedStrings #-}

module LogEntryParseTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions =
        [ ("TODO", ["DONE", "WAIT"])
        , ("WAIT", ["TODO", "DONE"])
        ]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

parseFile :: ByteString -> Either String OrgFile
parseFile bs = either (Left . snd) Right (parseOrgFile parseConfig "t.org" bs)

onlyEntry :: ByteString -> Either String Entry
onlyEntry bs = case parseFile bs of
  Right (OrgFile _ _ [e]) -> Right e
  Right _ -> Left "expected exactly one entry"
  Left msg -> Left msg

expectRight :: Either String a -> IO a
expectRight = either assertFailure pure

-- Predicate matchers over LogEntry without pulling in Data.Typeable or lenses.
logCtorName :: LogEntry -> String
logCtorName (LogClosing _ _ _) = "LogClosing"
logCtorName (LogState _ _ _ _ _) = "LogState"
logCtorName (LogNote _ _ _) = "LogNote"
logCtorName (LogRescheduled _ _ _ _) = "LogRescheduled"
logCtorName (LogNotScheduled _ _ _ _) = "LogNotScheduled"
logCtorName (LogDeadline _ _ _ _) = "LogDeadline"
logCtorName (LogNoDeadline _ _ _ _) = "LogNoDeadline"
logCtorName (LogRefiling _ _ _) = "LogRefiling"
logCtorName (LogClock _ _ _) = "LogClock"
logCtorName (LogBook _ _) = "LogBook"

tests :: TestTree
tests =
  testGroup
    "parseLogEntry"
    [ testCase "LogClosing without body" $ do
        e <- expectRight $ onlyEntry closingNoBody
        map logCtorName (_entryLogEntries e) @?= ["LogClosing"]
    , testCase "LogClosing with trailing note body" $ do
        e <- expectRight $ onlyEntry closingWithBody
        case _entryLogEntries e of
          [LogClosing _ _ (Just _)] -> pure ()
          other ->
            assertFailure ("expected LogClosing with body, got " ++ show other)
    , testCase "LogState with no previous keyword" $ do
        e <- expectRight $ onlyEntry stateNoFrom
        case _entryLogEntries e of
          [LogState _ _ Nothing _ _] -> pure ()
          other -> assertFailure ("expected LogState w/o from, got " ++ show other)
    , testCase "LogState with previous keyword" $ do
        e <- expectRight $ onlyEntry stateWithFrom
        case _entryLogEntries e of
          [LogState _ _ (Just _) _ _] -> pure ()
          other -> assertFailure ("expected LogState with from, got " ++ show other)
    , testCase "LogNote" $ do
        e <- expectRight $ onlyEntry noteTaken
        map logCtorName (_entryLogEntries e) @?= ["LogNote"]
    , testCase "LogRescheduled" $ do
        e <- expectRight $ onlyEntry rescheduled
        map logCtorName (_entryLogEntries e) @?= ["LogRescheduled"]
    , testCase "LogNotScheduled" $ do
        e <- expectRight $ onlyEntry notScheduled
        map logCtorName (_entryLogEntries e) @?= ["LogNotScheduled"]
    , testCase "LogDeadline" $ do
        e <- expectRight $ onlyEntry deadlineChanged
        map logCtorName (_entryLogEntries e) @?= ["LogDeadline"]
    , testCase "LogNoDeadline" $ do
        e <- expectRight $ onlyEntry deadlineRemoved
        map logCtorName (_entryLogEntries e) @?= ["LogNoDeadline"]
    , testCase "LogRefiling" $ do
        e <- expectRight $ onlyEntry refiled
        map logCtorName (_entryLogEntries e) @?= ["LogRefiling"]
    , testCase "LogClock without duration" $ do
        e <- expectRight $ onlyEntry clockOpen
        case _entryLogEntries e of
          [LogClock _ _ Nothing] -> pure ()
          other -> assertFailure ("expected open LogClock, got " ++ show other)
    , testCase "LogClock with duration" $ do
        e <- expectRight $ onlyEntry clockClosed
        case _entryLogEntries e of
          [LogClock _ _ (Just (Duration 1 0))] -> pure ()
          other -> assertFailure ("expected closed LogClock with 1:00, got " ++ show other)
    , testCase "LogBook wraps child log entries" $ do
        e <- expectRight $ onlyEntry logbookWrapper
        case _entryLogEntries e of
          [LogBook _ children] ->
            map logCtorName children @?= ["LogClock", "LogNote"]
          other -> assertFailure ("expected LogBook, got " ++ show other)
    , testCase "Refiling with body note" $ do
        e <- expectRight $ onlyEntry refilingWithBody
        case _entryLogEntries e of
          [LogRefiling _ _ (Just _)] -> pure ()
          other -> assertFailure ("expected LogRefiling with body, got " ++ show other)
    ]

closingNoBody :: ByteString
closingNoBody =
  fromLines
    [ "* DONE Task"
    , "- CLOSING NOTE [2024-01-15 Mon 17:00]"
    ]

closingWithBody :: ByteString
closingWithBody =
  fromLines
    [ "* DONE Task"
    , "- CLOSING NOTE [2024-01-15 Mon 17:00] \\\\"
    , "  A closing explanation."
    ]

stateNoFrom :: ByteString
stateNoFrom =
  fromLines
    [ "* DONE Task"
    , "- State \"DONE\"       [2024-01-15 Mon 17:00]"
    ]

stateWithFrom :: ByteString
stateWithFrom =
  fromLines
    [ "* DONE Task"
    , "- State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 17:00]"
    ]

noteTaken :: ByteString
noteTaken =
  fromLines
    [ "* TODO Task"
    , "- Note taken on [2024-01-10 Wed 11:00]"
    ]

rescheduled :: ByteString
rescheduled =
  fromLines
    [ "* TODO Task"
    , "- Rescheduled from \"[2024-01-10 Wed]\" on [2024-01-11 Thu 09:00]"
    ]

notScheduled :: ByteString
notScheduled =
  fromLines
    [ "* TODO Task"
    , "- Not scheduled, was \"[2024-01-10 Wed]\" on [2024-01-11 Thu 09:00]"
    ]

deadlineChanged :: ByteString
deadlineChanged =
  fromLines
    [ "* TODO Task"
    , "- New deadline from \"[2024-01-10 Wed]\" on [2024-01-11 Thu 09:00]"
    ]

deadlineRemoved :: ByteString
deadlineRemoved =
  fromLines
    [ "* TODO Task"
    , "- Removed deadline, was \"[2024-01-10 Wed]\" on [2024-01-11 Thu 09:00]"
    ]

refiled :: ByteString
refiled =
  fromLines
    [ "* TODO Task"
    , "- Refiled on [2024-01-15 Mon 09:00]"
    ]

refilingWithBody :: ByteString
refilingWithBody =
  fromLines
    [ "* TODO Task"
    , "- Refiled on [2024-01-15 Mon 09:00] \\\\"
    , "  Moved over from other file."
    ]

clockOpen :: ByteString
clockOpen =
  fromLines
    [ "* TODO Task"
    , "CLOCK: [2024-01-15 Mon 09:00]"
    ]

clockClosed :: ByteString
clockClosed =
  fromLines
    [ "* TODO Task"
    , "CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 10:00] =>  1:00"
    ]

logbookWrapper :: ByteString
logbookWrapper =
  fromLines
    [ "* TODO Task"
    , ":LOGBOOK:"
    , "CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 10:00] =>  1:00"
    , "- Note taken on [2024-01-15 Mon 10:05]"
    , ":END:"
    ]
