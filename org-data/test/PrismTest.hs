{-# LANGUAGE OverloadedStrings #-}

module PrismTest (tests) where

import Control.DeepSeq (force)
import Control.Lens (has, review, (#), (^?))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = force (Loc "p.org" 0)

mkTime :: Integer -> Time
mkTime day =
  force
    Time
      { _timeKind = InactiveTime
      , _timeDay = day
      , _timeDayEnd = Nothing
      , _timeStart = Nothing
      , _timeEnd = Nothing
      , _timeSuffix = Nothing
      }

t0 :: Time
t0 = mkTime 60000

t1 :: Time
t1 = mkTime 60001

tests :: TestTree
tests =
  testGroup
    "prisms"
    [ testGroup
        "Stamp prisms"
        [ testCase "_ClosedStamp matches ClosedStamp" $
            force (ClosedStamp loc0 t0) ^? _ClosedStamp @?= Just (loc0, t0)
        , testCase "_ClosedStamp does not match ScheduledStamp" $
            force (ScheduledStamp loc0 t0) ^? _ClosedStamp @?= Nothing
        , testCase "_ClosedStamp builds via review" $
            _ClosedStamp # (loc0, t0) @?= force (ClosedStamp loc0 t0)
        , testCase "_ScheduledStamp matches ScheduledStamp" $
            force (ScheduledStamp loc0 t0) ^? _ScheduledStamp @?= Just (loc0, t0)
        , testCase "_ScheduledStamp builds" $
            review _ScheduledStamp (loc0, t0) @?= force (ScheduledStamp loc0 t0)
        , testCase "_DeadlineStamp matches DeadlineStamp" $
            force (DeadlineStamp loc0 t0) ^? _DeadlineStamp @?= Just (loc0, t0)
        , testCase "_DeadlineStamp builds" $
            _DeadlineStamp # (loc0, t0) @?= force (DeadlineStamp loc0 t0)
        , testCase "_ActiveStamp matches ActiveStamp" $
            force (ActiveStamp loc0 t0) ^? _ActiveStamp @?= Just (loc0, t0)
        , testCase "_ActiveStamp builds" $
            _ActiveStamp # (loc0, t0) @?= force (ActiveStamp loc0 t0)
        , testCase "_ActiveStamp mismatch" $
            force (ClosedStamp loc0 t0) ^? _ActiveStamp @?= Nothing
        ]
    , testGroup
        "Keyword prisms"
        [ testCase "_OpenKeyword matches OpenKeyword" $
            force (OpenKeyword loc0 "TODO") ^? _OpenKeyword
              @?= Just (loc0, "TODO")
        , testCase "_OpenKeyword does not match ClosedKeyword" $
            force (ClosedKeyword loc0 "DONE") ^? _OpenKeyword @?= Nothing
        , testCase "_OpenKeyword builds" $
            _OpenKeyword # (loc0, "WAIT") @?= force (OpenKeyword loc0 "WAIT")
        , testCase "_ClosedKeyword matches ClosedKeyword" $
            force (ClosedKeyword loc0 "DONE") ^? _ClosedKeyword
              @?= Just (loc0, "DONE")
        , testCase "_ClosedKeyword builds" $
            _ClosedKeyword # (loc0, "CANCELED")
              @?= force (ClosedKeyword loc0 "CANCELED")
        , testCase "_ClosedKeyword mismatch" $
            force (OpenKeyword loc0 "TODO") ^? _ClosedKeyword @?= Nothing
        ]
    , testGroup
        "Tag prism"
        [ testCase "_PlainTag matches PlainTag" $
            force (PlainTag "x") ^? _PlainTag @?= Just "x"
        , testCase "_PlainTag builds" $
            _PlainTag # "foo" @?= force (PlainTag "foo")
        ]
    , testGroup
        "LogEntry prisms"
        [ testCase "_LogClosing matches LogClosing" $
            has _LogClosing (force (LogClosing loc0 t0 Nothing)) @?= True
        , testCase "_LogClosing preview" $
            force (LogClosing loc0 t0 Nothing) ^? _LogClosing
              @?= Just (loc0, t0, Nothing)
        , testCase "_LogClosing mismatch" $
            has _LogClosing (force (LogNote loc0 t0 Nothing)) @?= False
        , testCase "_LogState matches LogState" $
            let kw = force (OpenKeyword loc0 "TODO")
             in has _LogState (force (LogState loc0 kw Nothing t0 Nothing))
                  @?= True
        , testCase "_LogNote matches LogNote" $
            has _LogNote (force (LogNote loc0 t0 Nothing)) @?= True
        , testCase "_LogRescheduled matches LogRescheduled" $
            has _LogRescheduled (force (LogRescheduled loc0 t0 t1 Nothing))
              @?= True
        , testCase "_LogNotScheduled matches LogNotScheduled" $
            has _LogNotScheduled (force (LogNotScheduled loc0 t0 t1 Nothing))
              @?= True
        , testCase "_LogDeadline matches LogDeadline" $
            has _LogDeadline (force (LogDeadline loc0 t0 t1 Nothing)) @?= True
        , testCase "_LogNoDeadline matches LogNoDeadline" $
            has _LogNoDeadline (force (LogNoDeadline loc0 t0 t1 Nothing))
              @?= True
        , testCase "_LogRefiling matches LogRefiling" $
            has _LogRefiling (force (LogRefiling loc0 t0 Nothing)) @?= True
        , testCase "_LogClock matches LogClock" $
            has _LogClock (force (LogClock loc0 t0 Nothing)) @?= True
        , testCase "_LogBook matches LogBook" $
            has _LogBook (force (LogBook loc0 [])) @?= True
        , testCase "_LogBook preview yields (loc, entries)" $
            force (LogBook loc0 []) ^? _LogBook @?= Just (loc0, [])
        ]
    , testGroup
        "TimeSpan prisms"
        [ testCase "_DaySpan matches DaySpan" $
            has _DaySpan DaySpan @?= True
        , testCase "_DaySpan does not match WeekSpan" $
            has _DaySpan WeekSpan @?= False
        , testCase "_WeekSpan matches WeekSpan" $
            has _WeekSpan WeekSpan @?= True
        , testCase "_MonthSpan matches MonthSpan" $
            has _MonthSpan MonthSpan @?= True
        , testCase "_YearSpan matches YearSpan" $
            has _YearSpan YearSpan @?= True
        , testCase "_DaySpan builds" $
            review _DaySpan () @?= DaySpan
        , testCase "_WeekSpan builds" $
            review _WeekSpan () @?= WeekSpan
        , testCase "_MonthSpan builds" $
            review _MonthSpan () @?= MonthSpan
        , testCase "_YearSpan builds" $
            review _YearSpan () @?= YearSpan
        ]
    , testGroup
        "TimeKind prisms"
        [ testCase "_ActiveTime matches" $
            has _ActiveTime ActiveTime @?= True
        , testCase "_ActiveTime does not match InactiveTime" $
            has _ActiveTime InactiveTime @?= False
        , testCase "_InactiveTime matches" $
            has _InactiveTime InactiveTime @?= True
        , testCase "_ActiveTime builds" $
            review _ActiveTime () @?= ActiveTime
        , testCase "_InactiveTime builds" $
            review _InactiveTime () @?= InactiveTime
        ]
    , testGroup
        "TimeSuffixKind prisms"
        [ testCase "_TimeRepeat matches" $
            has _TimeRepeat TimeRepeat @?= True
        , testCase "_TimeRepeat does not match TimeDottedRepeat" $
            has _TimeRepeat TimeDottedRepeat @?= False
        , testCase "_TimeDottedRepeat matches" $
            has _TimeDottedRepeat TimeDottedRepeat @?= True
        , testCase "_TimeRepeatPlus matches" $
            has _TimeRepeatPlus TimeRepeatPlus @?= True
        , testCase "_TimeWithin matches" $
            has _TimeWithin TimeWithin @?= True
        , testCase "_TimeRepeat builds" $
            review _TimeRepeat () @?= TimeRepeat
        , testCase "_TimeRepeatPlus builds" $
            review _TimeRepeatPlus () @?= TimeRepeatPlus
        , testCase "_TimeDottedRepeat builds" $
            review _TimeDottedRepeat () @?= TimeDottedRepeat
        , testCase "_TimeWithin builds" $
            review _TimeWithin () @?= TimeWithin
        ]
    , testGroup
        "DrawerType prisms"
        [ testCase "_PlainDrawer matches PlainDrawer" $
            force (PlainDrawer "LOGBOOK") ^? _PlainDrawer @?= Just "LOGBOOK"
        , testCase "_PlainDrawer does not match BeginDrawer" $
            force (BeginDrawer "CUSTOM") ^? _PlainDrawer @?= Nothing
        , testCase "_BeginDrawer matches BeginDrawer" $
            force (BeginDrawer "CUSTOM") ^? _BeginDrawer @?= Just "CUSTOM"
        , testCase "_PlainDrawer builds" $
            _PlainDrawer # "x" @?= force (PlainDrawer "x")
        , testCase "_BeginDrawer builds" $
            _BeginDrawer # "y" @?= force (BeginDrawer "y")
        ]
    , testGroup
        "Block prisms"
        [ testCase "preview a Whitespace block" $
            force (Whitespace loc0 "  ") ^? _Whitespace @?= Just (loc0, "  ")
        , testCase "preview a Paragraph block" $
            force (Paragraph loc0 ["l1", "l2"]) ^? _Paragraph
              @?= Just (loc0, ["l1", "l2"])
        , testCase "preview a Drawer block" $
            force (Drawer loc0 (PlainDrawer "D") ["a"]) ^? _Drawer
              @?= Just (loc0, PlainDrawer "D", ["a"])
        ]
    , testGroup
        "TimeSuffix lens accessors"
        [ testCase "suffixKind gets" $
            let s = force (TimeSuffix TimeRepeat 1 DaySpan Nothing)
             in _suffixKind s @?= TimeRepeat
        , testCase "suffixNum gets" $
            let s = force (TimeSuffix TimeRepeat 3 DaySpan Nothing)
             in _suffixNum s @?= 3
        , testCase "suffixSpan gets" $
            let s = force (TimeSuffix TimeRepeat 1 WeekSpan Nothing)
             in _suffixSpan s @?= WeekSpan
        , testCase "suffixLargerSpan gets" $
            let s = force (TimeSuffix TimeRepeat 1 DaySpan (Just (2, MonthSpan)))
             in _suffixLargerSpan s @?= Just (2, MonthSpan)
        ]
    ]
