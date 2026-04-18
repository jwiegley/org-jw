{-# LANGUAGE OverloadedStrings #-}

module PlatedTest (tests) where

import Control.DeepSeq (force)
import Control.Lens (cosmos, toListOf, transform, universe)
import Org.Data ()
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = force (Loc "p.org" 0)

emptyBody0 :: Body
emptyBody0 = force (Body [])

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

-- A base entry (used where we need to nest entries to make non-trivial
-- Plated values).
mkEntry :: String -> [Entry] -> Entry
mkEntry title kids =
  force
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
            length (universe (force defaultConfig)) >= 1 @?= True
        , testCase "Loc" $
            length (universe (force loc0)) >= 1 @?= True
        , testCase "Property" $
            length (universe (force (Property loc0 False "k" "v"))) >= 1 @?= True
        , testCase "DrawerType" $
            length (universe (force (PlainDrawer "L"))) >= 1 @?= True
        , testCase "Block" $
            length (universe (force (Paragraph loc0 ["a"]))) >= 1 @?= True
        , testCase "Body (empty)" $
            length (universe (force emptyBody0)) >= 1 @?= True
        , testCase "Tag" $
            let x = force (PlainTag "foo")
             in x `seq` length (universe x) >= 1 @?= True
        , testCase "TimeSpan" $
            length (universe (force DaySpan)) >= 1 @?= True
        , testCase "TimeKind" $
            length (universe (force ActiveTime)) >= 1 @?= True
        , testCase "TimeSuffixKind" $
            length (universe (force TimeRepeat)) >= 1 @?= True
        , testCase "TimeSuffix" $
            length
              (universe (force (TimeSuffix TimeRepeat 1 DaySpan Nothing)))
              >= 1
              @?= True
        , testCase "Time" $
            length (universe (force (mkTime 60000))) >= 1 @?= True
        , testCase "Duration" $
            length (universe (force (Duration 1 30))) >= 1 @?= True
        , testCase "Stamp" $
            length (universe (force (ClosedStamp loc0 (mkTime 60000)))) >= 1 @?= True
        , testCase "Header" $
            length (universe (force (Header [] [] emptyBody0))) >= 1 @?= True
        , testCase "Keyword" $
            length (universe (force (OpenKeyword loc0 "TODO"))) >= 1 @?= True
        , testCase "LogEntry" $
            length (universe (force (LogNote loc0 (mkTime 60000) Nothing))) >= 1 @?= True
        , testCase "Entry" $
            length (universe (force (mkEntry "root" []))) >= 1 @?= True
        , testCase "OrgFile" $
            length
              (universe (force (OrgFile "x.org" (Header [] [] emptyBody0) [])))
              >= 1
              @?= True
        , testCase "CollectionItem" $
            length (universe (force (DataItem "x.png"))) >= 1 @?= True
        , testCase "Collection" $
            let x = force (Collection [])
             in x `seq` length (universe x) >= 1 @?= True
        ]
    , testGroup
        "cosmos collects elements"
        [ testCase "Entry with nested children" $
            let e =
                  force
                    ( mkEntry
                        "root"
                        [mkEntry "mid" [mkEntry "leaf" []], mkEntry "side" []]
                    )
             in length (toListOf cosmos e) @?= 4
        , testCase "LogBook enumerates itself + children" $
            let lb =
                  force
                    ( LogBook
                        loc0
                        [ LogNote loc0 (mkTime 60000) Nothing
                        , LogNote loc0 (mkTime 60001) Nothing
                        ]
                    )
             in length (toListOf cosmos lb) @?= 3
        , testCase "Body with nested blocks" $
            let b =
                  force
                    ( Body
                        [ Whitespace loc0 "  "
                        , Paragraph loc0 ["a"]
                        ]
                    )
             in length (toListOf cosmos b) @?= 1
        , testCase "Collection with nested OrgFile" $
            let c =
                  force
                    ( Collection
                        [ OrgItem
                            ( OrgFile
                                "a.org"
                                (Header [] [] emptyBody0)
                                [mkEntry "x" []]
                            )
                        , DataItem "y.png"
                        ]
                    )
             in c `seq` length (toListOf cosmos c) @?= 1
        ]
    , testGroup
        "transform id is identity"
        [ testCase "Entry" $
            transform id (force (mkEntry "a" [mkEntry "b" []]))
              @?= force (mkEntry "a" [mkEntry "b" []])
        , testCase "Body" $
            transform id (force (Body [Paragraph loc0 ["x"]]))
              @?= force (Body [Paragraph loc0 ["x"]])
        , testCase "OrgFile" $
            let of_ = force (OrgFile "z.org" (Header [] [] emptyBody0) [])
             in transform id of_ @?= of_
        , testCase "Collection" $
            let c = force (Collection [DataItem "x", DataItem "y"])
             in transform id c @?= c
        , testCase "LogBook" $
            let lb = force (LogBook loc0 [LogNote loc0 (mkTime 60000) Nothing])
             in transform id lb @?= lb
        , testCase "Tag" $
            transform id (force (PlainTag "foo")) @?= force (PlainTag "foo")
        , testCase "TimeSpan" $
            transform id (force DaySpan) @?= DaySpan
        , testCase "TimeKind" $
            transform id (force ActiveTime) @?= ActiveTime
        , testCase "TimeSuffixKind" $
            transform id (force TimeRepeat) @?= TimeRepeat
        , testCase "TimeSuffix" $
            transform id (force (TimeSuffix TimeRepeat 1 DaySpan Nothing))
              @?= force (TimeSuffix TimeRepeat 1 DaySpan Nothing)
        , testCase "Time" $
            transform id (force (mkTime 60000)) @?= force (mkTime 60000)
        , testCase "Duration" $
            transform id (force (Duration 1 30)) @?= force (Duration 1 30)
        , testCase "Header" $
            transform id (force (Header [] [] emptyBody0))
              @?= force (Header [] [] emptyBody0)
        , testCase "Keyword" $
            transform id (force (OpenKeyword loc0 "TODO"))
              @?= force (OpenKeyword loc0 "TODO")
        , testCase "Property" $
            transform id (force (Property loc0 False "k" "v"))
              @?= force (Property loc0 False "k" "v")
        , testCase "DrawerType" $
            transform id (force (BeginDrawer "X")) @?= force (BeginDrawer "X")
        , testCase "Loc" $
            transform id (force loc0) @?= force loc0
        , testCase "Config" $
            transform id (force defaultConfig) @?= force defaultConfig
        , testCase "Stamp" $
            transform id (force (ClosedStamp loc0 (mkTime 60000)))
              @?= force (ClosedStamp loc0 (mkTime 60000))
        , testCase "CollectionItem" $
            transform id (force (DataItem "x")) @?= force (DataItem "x")
        , testCase "Block" $
            transform id (force (Whitespace loc0 "  "))
              @?= force (Whitespace loc0 "  ")
        ]
    ]
