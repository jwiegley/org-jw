{-# LANGUAGE OverloadedStrings #-}

module StampTimeTest (tests) where

import Control.Lens ((&), (.~), (^..), (^?))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "t.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkTime :: Integer -> Time
mkTime day =
  Time
    { _timeKind = InactiveTime
    , _timeDay = day
    , _timeDayEnd = Nothing
    , _timeStart = Nothing
    , _timeEnd = Nothing
    , _timeSuffix = Nothing
    }

mkEntryWithStamps :: [Stamp] -> Entry
mkEntryWithStamps stamps =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "h"
    , _entryVerb = Nothing
    , _entryTitle = "h"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = stamps
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

mkEntryWithProps :: [Property] -> Entry
mkEntryWithProps ps =
  (mkEntryWithStamps []){_entryProperties = ps}

t1, t2 :: Time
t1 = mkTime 60000
t2 = mkTime 60010

tests :: TestTree
tests =
  testGroup
    "stamp time traversals"
    [ testGroup
        "closedTime"
        [ testCase "gets time from ClosedStamp" $
            mkEntryWithStamps [ClosedStamp loc0 t1] ^? closedTime @?= Just t1
        , testCase "returns Nothing without ClosedStamp" $
            mkEntryWithStamps [ScheduledStamp loc0 t1] ^? closedTime @?= Nothing
        , testCase "empty stamp list returns nothing" $
            mkEntryWithStamps [] ^? closedTime @?= Nothing
        , testCase "sets new closed time" $
            (mkEntryWithStamps [ClosedStamp loc0 t1] & closedTime .~ t2)
              ^? closedTime
              @?= Just t2
        , testCase "collects multiple ClosedStamps" $
            mkEntryWithStamps
              [ClosedStamp loc0 t1, ClosedStamp loc0 t2]
              ^.. closedTime
              @?= [t1, t2]
        ]
    , testGroup
        "scheduledTime"
        [ testCase "gets time from ScheduledStamp" $
            mkEntryWithStamps [ScheduledStamp loc0 t1] ^? scheduledTime @?= Just t1
        , testCase "returns Nothing without ScheduledStamp" $
            mkEntryWithStamps [ClosedStamp loc0 t1] ^? scheduledTime @?= Nothing
        , testCase "updates scheduled time" $
            (mkEntryWithStamps [ScheduledStamp loc0 t1] & scheduledTime .~ t2)
              ^? scheduledTime
              @?= Just t2
        ]
    , testGroup
        "deadlineTime"
        [ testCase "gets time from DeadlineStamp" $
            mkEntryWithStamps [DeadlineStamp loc0 t1] ^? deadlineTime @?= Just t1
        , testCase "returns Nothing without DeadlineStamp" $
            mkEntryWithStamps [ScheduledStamp loc0 t1] ^? deadlineTime @?= Nothing
        , testCase "sets deadline time" $
            (mkEntryWithStamps [DeadlineStamp loc0 t1] & deadlineTime .~ t2)
              ^? deadlineTime
              @?= Just t2
        ]
    , testGroup
        "multiple stamp kinds co-exist"
        [ testCase "mixed stamps - closed finds only ClosedStamp" $
            mkEntryWithStamps
              [ ScheduledStamp loc0 t1
              , ClosedStamp loc0 t2
              , DeadlineStamp loc0 t1
              ]
              ^? closedTime
              @?= Just t2
        , testCase "mixed stamps - scheduled finds only ScheduledStamp" $
            mkEntryWithStamps
              [ ScheduledStamp loc0 t1
              , ClosedStamp loc0 t2
              ]
              ^? scheduledTime
              @?= Just t1
        ]
    , testGroup
        "createdTime"
        [ testCase "parses CREATED property as Time" $
            let created = "[2024-06-01 Sat]"
                e = mkEntryWithProps [Property loc0 False "CREATED" created]
             in (e ^? createdTime) /= Nothing @?= True
        , testCase "returns Nothing without CREATED property" $
            mkEntryWithProps [] ^? createdTime @?= Nothing
        , testCase "returns Nothing on malformed value" $
            let e = mkEntryWithProps [Property loc0 False "CREATED" "not-a-date"]
             in e ^? createdTime @?= Nothing
        ]
    , testGroup
        "editedTime"
        [ testCase "parses EDITED property as Time" $
            let e = mkEntryWithProps [Property loc0 False "EDITED" "[2024-06-01 Sat]"]
             in (e ^? editedTime) /= Nothing @?= True
        , testCase "returns Nothing without EDITED property" $
            mkEntryWithProps [] ^? editedTime @?= Nothing
        , testCase "returns Nothing on malformed value" $
            let e = mkEntryWithProps [Property loc0 False "EDITED" "xxx"]
             in e ^? editedTime @?= Nothing
        ]
    ]
