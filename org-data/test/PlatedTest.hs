{-# LANGUAGE OverloadedStrings #-}

module PlatedTest (tests) where

import Control.Lens (cosmos, toListOf, transform, universe)
import Org.Data ()
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "p.org" 0

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

-- A base entry (used where we need to nest entries to make non-trivial
-- Plated values).
mkEntry :: String -> [Entry] -> Entry
mkEntry title kids =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = title
    , _entryVerb = Nothing
    , _entryTitle = title
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = kids
    }

tests :: TestTree
tests =
  testGroup
    "Plated instances"
    [ testGroup
        "universe is non-empty"
        [ testCase "Config" $
            length (universe defaultConfig) >= 1 @?= True
        , testCase "Loc" $
            length (universe loc0) >= 1 @?= True
        , testCase "Property" $
            length (universe (Property loc0 False "k" "v")) >= 1 @?= True
        , testCase "DrawerType" $
            length (universe (PlainDrawer "L")) >= 1 @?= True
        , testCase "Block" $
            length (universe (Paragraph loc0 ["a"])) >= 1 @?= True
        , testCase "Body (empty)" $
            length (universe emptyBody0) >= 1 @?= True
        , testCase "Tag" $
            length (universe (PlainTag "foo")) >= 1 @?= True
        , testCase "TimeSpan" $
            length (universe DaySpan) >= 1 @?= True
        , testCase "TimeKind" $
            length (universe ActiveTime) >= 1 @?= True
        , testCase "TimeSuffixKind" $
            length (universe TimeRepeat) >= 1 @?= True
        , testCase "TimeSuffix" $
            length
              (universe (TimeSuffix TimeRepeat 1 DaySpan Nothing))
              >= 1
              @?= True
        , testCase "Time" $
            length (universe (mkTime 60000)) >= 1 @?= True
        , testCase "Duration" $
            length (universe (Duration 1 30)) >= 1 @?= True
        , testCase "Stamp" $
            length (universe (ClosedStamp loc0 (mkTime 60000))) >= 1 @?= True
        , testCase "Header" $
            length (universe (Header [] [] emptyBody0)) >= 1 @?= True
        , testCase "Keyword" $
            length (universe (OpenKeyword loc0 "TODO")) >= 1 @?= True
        , testCase "LogEntry" $
            length (universe (LogNote loc0 (mkTime 60000) Nothing)) >= 1 @?= True
        , testCase "Entry" $
            length (universe (mkEntry "root" [])) >= 1 @?= True
        , testCase "OrgFile" $
            length
              (universe (OrgFile "x.org" (Header [] [] emptyBody0) []))
              >= 1
              @?= True
        , testCase "CollectionItem" $
            length (universe (DataItem "x.png")) >= 1 @?= True
        , testCase "Collection" $
            length (universe (Collection [])) >= 1 @?= True
        ]
    , testGroup
        "cosmos collects elements"
        [ testCase "Entry with nested children" $
            let e =
                  mkEntry
                    "root"
                    [mkEntry "mid" [mkEntry "leaf" []], mkEntry "side" []]
             in length (toListOf cosmos e) @?= 4
        , testCase "LogBook enumerates itself + children" $
            let lb =
                  LogBook
                    loc0
                    [ LogNote loc0 (mkTime 60000) Nothing
                    , LogNote loc0 (mkTime 60001) Nothing
                    ]
             in length (toListOf cosmos lb) @?= 3
        , testCase "Body with nested blocks" $
            let b =
                  Body
                    [ Whitespace loc0 "  "
                    , Paragraph loc0 ["a"]
                    ]
             in length (toListOf cosmos b) @?= 1
        , testCase "Collection with nested OrgFile" $
            let c =
                  Collection
                    [ OrgItem
                        ( OrgFile
                            "a.org"
                            (Header [] [] emptyBody0)
                            [mkEntry "x" []]
                        )
                    , DataItem "y.png"
                    ]
             in length (toListOf cosmos c) @?= 1
        ]
    , testGroup
        "transform id is identity"
        [ testCase "Entry" $
            transform id (mkEntry "a" [mkEntry "b" []])
              @?= mkEntry "a" [mkEntry "b" []]
        , testCase "Body" $
            transform id (Body [Paragraph loc0 ["x"]])
              @?= Body [Paragraph loc0 ["x"]]
        , testCase "OrgFile" $
            let of_ = OrgFile "z.org" (Header [] [] emptyBody0) []
             in transform id of_ @?= of_
        , testCase "Collection" $
            let c = Collection [DataItem "x", DataItem "y"]
             in transform id c @?= c
        , testCase "LogBook" $
            let lb = LogBook loc0 [LogNote loc0 (mkTime 60000) Nothing]
             in transform id lb @?= lb
        , testCase "Tag" $
            transform id (PlainTag "foo") @?= PlainTag "foo"
        , testCase "TimeSpan" $
            transform id DaySpan @?= DaySpan
        , testCase "TimeKind" $
            transform id ActiveTime @?= ActiveTime
        , testCase "TimeSuffixKind" $
            transform id TimeRepeat @?= TimeRepeat
        , testCase "TimeSuffix" $
            transform id (TimeSuffix TimeRepeat 1 DaySpan Nothing)
              @?= TimeSuffix TimeRepeat 1 DaySpan Nothing
        , testCase "Time" $
            transform id (mkTime 60000) @?= mkTime 60000
        , testCase "Duration" $
            transform id (Duration 1 30) @?= Duration 1 30
        , testCase "Header" $
            transform id (Header [] [] emptyBody0)
              @?= Header [] [] emptyBody0
        , testCase "Keyword" $
            transform id (OpenKeyword loc0 "TODO")
              @?= OpenKeyword loc0 "TODO"
        , testCase "Property" $
            transform id (Property loc0 False "k" "v")
              @?= Property loc0 False "k" "v"
        , testCase "DrawerType" $
            transform id (BeginDrawer "X") @?= BeginDrawer "X"
        , testCase "Loc" $
            transform id loc0 @?= loc0
        , testCase "Config" $
            transform id defaultConfig @?= defaultConfig
        , testCase "Stamp" $
            transform id (ClosedStamp loc0 (mkTime 60000))
              @?= ClosedStamp loc0 (mkTime 60000)
        , testCase "CollectionItem" $
            transform id (DataItem "x") @?= DataItem "x"
        , testCase "Block" $
            transform id (Whitespace loc0 "  ") @?= Whitespace loc0 "  "
        ]
    ]
