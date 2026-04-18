{-# LANGUAGE OverloadedStrings #-}

module FieldLensesTest (tests) where

import Control.Lens (
  preview,
  review,
  set,
  view,
  (#),
  (&),
  (.~),
  (?~),
  (^.),
 )
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "f.org" 0

loc1 :: Loc
loc1 = Loc "g.org" 12

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

sampleProperty :: Property
sampleProperty = Property loc0 False "ID" "abc"

sampleEntry :: Entry
sampleEntry =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 2
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "H"
    , _entryVerb = Nothing
    , _entryTitle = "T"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

sampleHeader :: Header
sampleHeader =
  Header
    { _headerPropertiesDrawer = [Property loc0 False "ID" "h-id"]
    , _headerFileProperties = [Property loc0 False "TITLE" "t"]
    , _headerPreamble = emptyBody0
    }

sampleOrgFile :: OrgFile
sampleOrgFile = OrgFile "x.org" sampleHeader [sampleEntry]

sampleCollection :: Collection
sampleCollection = Collection [OrgItem sampleOrgFile, DataItem "a.png"]

tests :: TestTree
tests =
  testGroup
    "field lenses and prisms"
    [ testGroup
        "Property field lenses"
        [ testCase "propertyLoc gets loc" $
            view propertyLoc sampleProperty @?= loc0
        , testCase "propertyLoc sets loc" $
            (sampleProperty & propertyLoc .~ loc1) ^. propertyLoc @?= loc1
        , testCase "inherited gets False" $
            view inherited sampleProperty @?= False
        , testCase "inherited sets True" $
            (sampleProperty & inherited .~ True) ^. inherited @?= True
        , testCase "name gets" $
            view name sampleProperty @?= "ID"
        , testCase "name sets" $
            (sampleProperty & name .~ "CATEGORY") ^. name @?= "CATEGORY"
        , testCase "value gets" $
            view value sampleProperty @?= "abc"
        , testCase "value sets" $
            (sampleProperty & value .~ "zzz") ^. value @?= "zzz"
        ]
    , testGroup
        "TimeSuffix field lenses"
        [ testCase "suffixKind gets" $
            view suffixKind (TimeSuffix TimeRepeat 1 DaySpan Nothing)
              @?= TimeRepeat
        , testCase "suffixKind sets" $
            view
              suffixKind
              ( set
                  suffixKind
                  TimeDottedRepeat
                  (TimeSuffix TimeRepeat 1 DaySpan Nothing)
              )
              @?= TimeDottedRepeat
        , testCase "suffixNum gets" $
            view suffixNum (TimeSuffix TimeRepeat 5 DaySpan Nothing) @?= 5
        , testCase "suffixNum sets" $
            view
              suffixNum
              ( set
                  suffixNum
                  9
                  (TimeSuffix TimeRepeat 5 DaySpan Nothing)
              )
              @?= 9
        , testCase "suffixSpan gets" $
            view suffixSpan (TimeSuffix TimeRepeat 1 WeekSpan Nothing)
              @?= WeekSpan
        , testCase "suffixSpan sets" $
            view
              suffixSpan
              ( set
                  suffixSpan
                  MonthSpan
                  (TimeSuffix TimeRepeat 1 WeekSpan Nothing)
              )
              @?= MonthSpan
        , testCase "suffixLargerSpan gets" $
            view
              suffixLargerSpan
              (TimeSuffix TimeRepeat 1 DaySpan (Just (2, YearSpan)))
              @?= Just (2, YearSpan)
        , testCase "suffixLargerSpan sets" $
            view
              suffixLargerSpan
              ( set
                  suffixLargerSpan
                  (Just (4, MonthSpan))
                  (TimeSuffix TimeRepeat 1 DaySpan Nothing)
              )
              @?= Just (4, MonthSpan)
        ]
    , testGroup
        "Duration classy lenses"
        [ testCase "hours gets" $
            view hours (Duration 3 15) @?= 3
        , testCase "hours sets" $
            view hours (set hours 7 (Duration 3 15)) @?= 7
        , testCase "mins gets" $
            view mins (Duration 3 15) @?= 15
        , testCase "mins sets" $
            view mins (set mins 45 (Duration 3 15)) @?= 45
        ]
    , testGroup
        "Entry classy lenses"
        [ testCase "entryDepth gets" $
            view entryDepth sampleEntry @?= 2
        , testCase "entryDepth sets" $
            view entryDepth (sampleEntry & entryDepth .~ 5) @?= 5
        , testCase "entryPriority gets Nothing" $
            view entryPriority sampleEntry @?= Nothing
        , testCase "entryPriority sets" $
            view
              entryPriority
              (sampleEntry & entryPriority ?~ "A")
              @?= Just "A"
        , testCase "entryVerb gets Nothing" $
            view entryVerb sampleEntry @?= Nothing
        , testCase "entryVerb sets" $
            view
              entryVerb
              (sampleEntry & entryVerb ?~ "Read")
              @?= Just "Read"
        , testCase "entryContext gets Nothing" $
            view entryContext sampleEntry @?= Nothing
        , testCase "entryContext sets" $
            view
              entryContext
              (sampleEntry & entryContext ?~ "home")
              @?= Just "home"
        , testCase "entryLocator gets Nothing" $
            view entryLocator sampleEntry @?= Nothing
        , testCase "entryLocator sets" $
            view
              entryLocator
              (sampleEntry & entryLocator ?~ "p42")
              @?= Just "p42"
        , testCase "entryStamps gets empty" $
            view entryStamps sampleEntry @?= []
        , testCase "entryStamps sets" $
            let st = [ClosedStamp loc0 (mkTime 60000)]
             in view entryStamps (sampleEntry & entryStamps .~ st) @?= st
        , testCase "entryLogEntries gets empty" $
            view entryLogEntries sampleEntry @?= []
        , testCase "entryLogEntries sets" $
            let les = [LogNote loc0 (mkTime 60000) Nothing]
             in view
                  entryLogEntries
                  (sampleEntry & entryLogEntries .~ les)
                  @?= les
        , testCase "entryItems gets empty" $
            view entryItems sampleEntry @?= []
        , testCase "entryItems sets" $
            let kid = sampleEntry{_entryHeadline = "kid"}
             in length
                  ( view
                      entryItems
                      (sampleEntry & entryItems .~ [kid])
                  )
                  @?= 1
        ]
    , testGroup
        "Header classy lenses"
        [ testCase "headerPreamble gets" $
            view headerPreamble sampleHeader @?= emptyBody0
        , testCase "headerPreamble sets" $
            let b = Body [Whitespace loc0 "  "]
             in view
                  headerPreamble
                  (sampleHeader & headerPreamble .~ b)
                  @?= b
        , testCase "headerFileProperties gets" $
            length (view headerFileProperties sampleHeader) @?= 1
        , testCase "headerFileProperties sets" $
            let ps = [Property loc0 False "A" "1", Property loc0 False "B" "2"]
             in length
                  ( view
                      headerFileProperties
                      (sampleHeader & headerFileProperties .~ ps)
                  )
                  @?= 2
        , testCase "headerPropertiesDrawer gets" $
            length (view headerPropertiesDrawer sampleHeader) @?= 1
        , testCase "headerPropertiesDrawer sets" $
            let ps = [Property loc0 False "X" "1"]
             in length
                  ( view
                      headerPropertiesDrawer
                      (sampleHeader & headerPropertiesDrawer .~ ps)
                  )
                  @?= 1
        ]
    , testGroup
        "OrgFile classy lenses"
        [ testCase "orgFileEntries gets" $
            length (view orgFileEntries sampleOrgFile) @?= 1
        , testCase "orgFileEntries sets" $
            length
              ( view
                  orgFileEntries
                  (sampleOrgFile & orgFileEntries .~ [])
              )
              @?= 0
        ]
    , testGroup
        "Collection classy lenses"
        [ testCase "items gets" $
            length (view items sampleCollection) @?= 2
        , testCase "items sets" $
            length
              ( view
                  items
                  (sampleCollection & items .~ [])
              )
              @?= 0
        ]
    , testGroup
        "Tag prism roundtrip"
        [ testCase "_PlainTag preview" $
            preview _PlainTag (PlainTag "x") @?= Just "x"
        , testCase "_PlainTag review" $
            review _PlainTag "y" @?= PlainTag "y"
        , testCase "_PlainTag review-then-preview" $
            preview _PlainTag (review _PlainTag "z") @?= Just "z"
        ]
    , testGroup
        "Keyword prisms roundtrip"
        [ testCase "_OpenKeyword preview" $
            preview _OpenKeyword (OpenKeyword loc0 "TODO")
              @?= Just (loc0, "TODO")
        , testCase "_OpenKeyword review" $
            _OpenKeyword # (loc0, "W") @?= OpenKeyword loc0 "W"
        , testCase "_ClosedKeyword preview" $
            preview _ClosedKeyword (ClosedKeyword loc0 "DONE")
              @?= Just (loc0, "DONE")
        , testCase "_ClosedKeyword review" $
            _ClosedKeyword # (loc0, "C") @?= ClosedKeyword loc0 "C"
        ]
    , testGroup
        "TimeSpan prisms roundtrip"
        [ testCase "_DaySpan preview" $
            preview _DaySpan DaySpan @?= Just ()
        , testCase "_WeekSpan preview" $
            preview _WeekSpan WeekSpan @?= Just ()
        , testCase "_MonthSpan preview" $
            preview _MonthSpan MonthSpan @?= Just ()
        , testCase "_YearSpan preview" $
            preview _YearSpan YearSpan @?= Just ()
        , testCase "_DaySpan review" $
            (_DaySpan # ()) @?= DaySpan
        , testCase "_WeekSpan review" $
            (_WeekSpan # ()) @?= WeekSpan
        , testCase "_MonthSpan review" $
            (_MonthSpan # ()) @?= MonthSpan
        , testCase "_YearSpan review" $
            (_YearSpan # ()) @?= YearSpan
        ]
    , testGroup
        "TimeKind prisms roundtrip"
        [ testCase "_ActiveTime preview" $
            preview _ActiveTime ActiveTime @?= Just ()
        , testCase "_InactiveTime preview" $
            preview _InactiveTime InactiveTime @?= Just ()
        , testCase "_ActiveTime review" $
            (_ActiveTime # ()) @?= ActiveTime
        , testCase "_InactiveTime review" $
            (_InactiveTime # ()) @?= InactiveTime
        ]
    , testGroup
        "TimeSuffixKind prisms roundtrip"
        [ testCase "_TimeRepeat preview" $
            preview _TimeRepeat TimeRepeat @?= Just ()
        , testCase "_TimeRepeatPlus preview" $
            preview _TimeRepeatPlus TimeRepeatPlus @?= Just ()
        , testCase "_TimeDottedRepeat preview" $
            preview _TimeDottedRepeat TimeDottedRepeat @?= Just ()
        , testCase "_TimeWithin preview" $
            preview _TimeWithin TimeWithin @?= Just ()
        , testCase "_TimeRepeat review" $
            (_TimeRepeat # ()) @?= TimeRepeat
        , testCase "_TimeRepeatPlus review" $
            (_TimeRepeatPlus # ()) @?= TimeRepeatPlus
        , testCase "_TimeDottedRepeat review" $
            (_TimeDottedRepeat # ()) @?= TimeDottedRepeat
        , testCase "_TimeWithin review" $
            (_TimeWithin # ()) @?= TimeWithin
        ]
    , testGroup
        "Stamp prisms roundtrip"
        [ testCase "_ClosedStamp preview on ClosedStamp" $
            preview _ClosedStamp (ClosedStamp loc0 (mkTime 60000))
              @?= Just (loc0, mkTime 60000)
        , testCase "_ClosedStamp preview on mismatch" $
            preview _ClosedStamp (ActiveStamp loc0 (mkTime 60000)) @?= Nothing
        , testCase "_ClosedStamp review" $
            _ClosedStamp # (loc0, mkTime 60000)
              @?= ClosedStamp loc0 (mkTime 60000)
        , testCase "_ScheduledStamp preview" $
            preview _ScheduledStamp (ScheduledStamp loc0 (mkTime 60000))
              @?= Just (loc0, mkTime 60000)
        , testCase "_ScheduledStamp review" $
            _ScheduledStamp # (loc0, mkTime 60000)
              @?= ScheduledStamp loc0 (mkTime 60000)
        , testCase "_DeadlineStamp preview" $
            preview _DeadlineStamp (DeadlineStamp loc0 (mkTime 60000))
              @?= Just (loc0, mkTime 60000)
        , testCase "_DeadlineStamp review" $
            _DeadlineStamp # (loc0, mkTime 60000)
              @?= DeadlineStamp loc0 (mkTime 60000)
        , testCase "_ActiveStamp preview" $
            preview _ActiveStamp (ActiveStamp loc0 (mkTime 60000))
              @?= Just (loc0, mkTime 60000)
        , testCase "_ActiveStamp review" $
            _ActiveStamp # (loc0, mkTime 60000)
              @?= ActiveStamp loc0 (mkTime 60000)
        ]
    , testGroup
        "DrawerType prisms roundtrip"
        [ testCase "_PlainDrawer preview" $
            preview _PlainDrawer (PlainDrawer "X") @?= Just "X"
        , testCase "_PlainDrawer review" $
            _PlainDrawer # "X" @?= PlainDrawer "X"
        , testCase "_BeginDrawer preview" $
            preview _BeginDrawer (BeginDrawer "Y") @?= Just "Y"
        , testCase "_BeginDrawer review" $
            _BeginDrawer # "Y" @?= BeginDrawer "Y"
        ]
    , testGroup
        "Block prisms"
        [ testCase "_Whitespace preview" $
            preview _Whitespace (Whitespace loc0 "  ")
              @?= Just (loc0, "  ")
        , testCase "_Whitespace review" $
            _Whitespace # (loc0, "  ") @?= Whitespace loc0 "  "
        , testCase "_Paragraph preview" $
            preview _Paragraph (Paragraph loc0 ["a"]) @?= Just (loc0, ["a"])
        , testCase "_Paragraph review" $
            _Paragraph # (loc0, ["a"]) @?= Paragraph loc0 ["a"]
        , testCase "_Drawer preview" $
            preview _Drawer (Drawer loc0 (PlainDrawer "D") ["l"])
              @?= Just (loc0, PlainDrawer "D", ["l"])
        , testCase "_Drawer review" $
            _Drawer # (loc0, PlainDrawer "D", ["l"])
              @?= Drawer loc0 (PlainDrawer "D") ["l"]
        , testCase "_InlineTask preview" $
            preview _InlineTask (InlineTask loc0 sampleEntry)
              @?= Just (loc0, sampleEntry)
        , testCase "_InlineTask review" $
            _InlineTask # (loc0, sampleEntry)
              @?= InlineTask loc0 sampleEntry
        ]
    , testGroup
        "CollectionItem prisms"
        [ testCase "_OrgItem preview" $
            preview _OrgItem (OrgItem sampleOrgFile) @?= Just sampleOrgFile
        , testCase "_OrgItem review" $
            _OrgItem # sampleOrgFile @?= OrgItem sampleOrgFile
        , testCase "_OrgItem preview on DataItem" $
            preview _OrgItem (DataItem "x.png") @?= Nothing
        , testCase "_DataItem preview" $
            preview _DataItem (DataItem "x.png") @?= Just "x.png"
        , testCase "_DataItem review" $
            _DataItem # "x.png" @?= DataItem "x.png"
        , testCase "_DataItem preview on OrgItem" $
            preview _DataItem (OrgItem sampleOrgFile) @?= Nothing
        ]
    , testGroup
        "LogEntry prisms roundtrip"
        [ testCase "_LogClosing preview" $
            preview _LogClosing (LogClosing loc0 (mkTime 60000) Nothing)
              @?= Just (loc0, mkTime 60000, Nothing)
        , testCase "_LogClosing review" $
            _LogClosing # (loc0, mkTime 60000, Nothing)
              @?= LogClosing loc0 (mkTime 60000) Nothing
        , testCase "_LogState preview" $
            let kw = OpenKeyword loc0 "TODO"
             in preview _LogState (LogState loc0 kw Nothing (mkTime 60000) Nothing)
                  @?= Just (loc0, kw, Nothing, mkTime 60000, Nothing)
        , testCase "_LogState review" $
            let kw = OpenKeyword loc0 "TODO"
             in _LogState # (loc0, kw, Nothing, mkTime 60000, Nothing)
                  @?= LogState loc0 kw Nothing (mkTime 60000) Nothing
        , testCase "_LogNote preview" $
            preview _LogNote (LogNote loc0 (mkTime 60000) Nothing)
              @?= Just (loc0, mkTime 60000, Nothing)
        , testCase "_LogNote review" $
            _LogNote # (loc0, mkTime 60000, Nothing)
              @?= LogNote loc0 (mkTime 60000) Nothing
        , testCase "_LogRescheduled preview" $
            preview
              _LogRescheduled
              (LogRescheduled loc0 (mkTime 60000) (mkTime 60001) Nothing)
              @?= Just (loc0, mkTime 60000, mkTime 60001, Nothing)
        , testCase "_LogRescheduled review" $
            _LogRescheduled # (loc0, mkTime 60000, mkTime 60001, Nothing)
              @?= LogRescheduled loc0 (mkTime 60000) (mkTime 60001) Nothing
        , testCase "_LogNotScheduled preview" $
            preview
              _LogNotScheduled
              (LogNotScheduled loc0 (mkTime 60000) (mkTime 60001) Nothing)
              @?= Just (loc0, mkTime 60000, mkTime 60001, Nothing)
        , testCase "_LogNotScheduled review" $
            _LogNotScheduled # (loc0, mkTime 60000, mkTime 60001, Nothing)
              @?= LogNotScheduled loc0 (mkTime 60000) (mkTime 60001) Nothing
        , testCase "_LogDeadline preview" $
            preview
              _LogDeadline
              (LogDeadline loc0 (mkTime 60000) (mkTime 60001) Nothing)
              @?= Just (loc0, mkTime 60000, mkTime 60001, Nothing)
        , testCase "_LogDeadline review" $
            _LogDeadline # (loc0, mkTime 60000, mkTime 60001, Nothing)
              @?= LogDeadline loc0 (mkTime 60000) (mkTime 60001) Nothing
        , testCase "_LogNoDeadline preview" $
            preview
              _LogNoDeadline
              (LogNoDeadline loc0 (mkTime 60000) (mkTime 60001) Nothing)
              @?= Just (loc0, mkTime 60000, mkTime 60001, Nothing)
        , testCase "_LogNoDeadline review" $
            _LogNoDeadline # (loc0, mkTime 60000, mkTime 60001, Nothing)
              @?= LogNoDeadline loc0 (mkTime 60000) (mkTime 60001) Nothing
        , testCase "_LogRefiling preview" $
            preview _LogRefiling (LogRefiling loc0 (mkTime 60000) Nothing)
              @?= Just (loc0, mkTime 60000, Nothing)
        , testCase "_LogRefiling review" $
            _LogRefiling # (loc0, mkTime 60000, Nothing)
              @?= LogRefiling loc0 (mkTime 60000) Nothing
        , testCase "_LogClock preview" $
            preview _LogClock (LogClock loc0 (mkTime 60000) Nothing)
              @?= Just (loc0, mkTime 60000, Nothing)
        , testCase "_LogClock review" $
            _LogClock # (loc0, mkTime 60000, Nothing)
              @?= LogClock loc0 (mkTime 60000) Nothing
        , testCase "_LogBook preview" $
            preview _LogBook (LogBook loc0 []) @?= Just (loc0, [])
        , testCase "_LogBook review" $
            _LogBook # (loc0, []) @?= LogBook loc0 []
        ]
    ]
