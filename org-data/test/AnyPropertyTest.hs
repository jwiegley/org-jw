{-# LANGUAGE OverloadedStrings #-}

module AnyPropertyTest (tests) where

import Control.Lens (toListOf, (^?))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "f.org" 0

loc1 :: Loc
loc1 = Loc "from-file.org" 42

emptyBody0 :: Body
emptyBody0 = Body []

cfg :: Config
cfg =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    }

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

baseEntry :: Entry
baseEntry =
  Entry
    { _entryLoc = loc1
    , _entryDepth = 3
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "Top task"
    , _entryVerb = Nothing
    , _entryTitle = "the title"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

tests :: TestTree
tests =
  testGroup
    "anyProperty"
    [ testGroup
        "real properties take precedence"
        [ testCase "real ID property is found" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "abc"]}
             in e ^? anyProperty cfg "ID" @?= Just "abc"
        , testCase "real FILE property takes precedence over virtual" $
            let e = baseEntry{_entryProperties = [Property loc0 False "FILE" "real"]}
             in e ^? anyProperty cfg "FILE" @?= Just "real"
        ]
    , testGroup
        "virtual properties from specialProperties"
        [ testCase "FILE returns entry loc file" $
            baseEntry ^? anyProperty cfg "FILE" @?= Just "from-file.org"
        , testCase "CATEGORY returns entry loc file" $
            baseEntry ^? anyProperty cfg "CATEGORY" @?= Just "from-file.org"
        , testCase "ITEM returns entry headline" $
            baseEntry ^? anyProperty cfg "ITEM" @?= Just "Top task"
        , testCase "TITLE returns entry title" $
            baseEntry ^? anyProperty cfg "TITLE" @?= Just "the title"
        , testCase "OFFSET returns entry loc pos as shown" $
            baseEntry ^? anyProperty cfg "OFFSET" @?= Just "42"
        , testCase "DEPTH returns entry depth as shown" $
            baseEntry ^? anyProperty cfg "DEPTH" @?= Just "3"
        , testCase "PRIORITY returns priority when present" $
            (baseEntry{_entryPriority = Just "A"}) ^? anyProperty cfg "PRIORITY"
              @?= Just "A"
        , testCase "PRIORITY returns Nothing when absent" $
            baseEntry ^? anyProperty cfg "PRIORITY" @?= Nothing
        , testCase "CONTEXT returns context when present" $
            (baseEntry{_entryContext = Just "home"}) ^? anyProperty cfg "CONTEXT"
              @?= Just "home"
        , testCase "CONTEXT returns Nothing when absent" $
            baseEntry ^? anyProperty cfg "CONTEXT" @?= Nothing
        , testCase "VERB returns verb when present" $
            (baseEntry{_entryVerb = Just "Read"}) ^? anyProperty cfg "VERB"
              @?= Just "Read"
        , testCase "LOCATOR returns locator when present" $
            (baseEntry{_entryLocator = Just "p42"}) ^? anyProperty cfg "LOCATOR"
              @?= Just "p42"
        , testCase "KEYWORD returns keyword string" $
            (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")})
              ^? anyProperty cfg "KEYWORD"
              @?= Just "TODO"
        , testCase "KEYWORD returns Nothing without keyword" $
            baseEntry ^? anyProperty cfg "KEYWORD" @?= Nothing
        , testCase "TODO returns keyword when it is a known todo" $
            (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")})
              ^? anyProperty cfg "TODO"
              @?= Just "TODO"
        , testCase "TODO returns Nothing when keyword not in config" $
            (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "OTHER")})
              ^? anyProperty cfg "TODO"
              @?= Nothing
        , testCase "TAGS returns serialized tag string" $
            let e = baseEntry{_entryTags = [PlainTag "foo", PlainTag "bar"]}
             in e ^? anyProperty cfg "TAGS" @?= Just "::foo:bar::"
        , testCase "TAGS on empty tags" $
            baseEntry ^? anyProperty cfg "TAGS" @?= Just ":::"
        ]
    , testGroup
        "stamp-based special properties"
        [ testCase "CLOSED returns Time-serialized string" $
            let e = baseEntry{_entryStamps = [ClosedStamp loc0 (mkTime 60000)]}
             in (e ^? anyProperty cfg "CLOSED") /= Nothing @?= True
        , testCase "CLOSED returns Nothing without ClosedStamp" $
            baseEntry ^? anyProperty cfg "CLOSED" @?= Nothing
        , testCase "SCHEDULED returns Time-serialized string" $
            let e = baseEntry{_entryStamps = [ScheduledStamp loc0 (mkTime 60000)]}
             in (e ^? anyProperty cfg "SCHEDULED") /= Nothing @?= True
        , testCase "DEADLINE returns Time-serialized string" $
            let e = baseEntry{_entryStamps = [DeadlineStamp loc0 (mkTime 60000)]}
             in (e ^? anyProperty cfg "DEADLINE") /= Nothing @?= True
        ]
    , testGroup
        "unknown properties"
        [ testCase "unknown property yields empty fold" $
            baseEntry ^? anyProperty cfg "NOT-A-THING" @?= Nothing
        , testCase "unknown property produces empty list" $
            toListOf (anyProperty cfg "DOES-NOT-EXIST") baseEntry @?= ([] :: [String])
        ]
    , testGroup
        "specialProperties table"
        [ testCase "contains known keys" $
            let keys = map fst (specialProperties cfg)
             in assertBool "FILE present" ("FILE" `elem` keys)
        , testCase "contains ITEM" $
            let keys = map fst (specialProperties cfg)
             in assertBool "ITEM present" ("ITEM" `elem` keys)
        , testCase "contains TODO" $
            let keys = map fst (specialProperties cfg)
             in assertBool "TODO present" ("TODO" `elem` keys)
        , testCase "contains CUSTOM keys" $
            let keys = map fst (specialProperties cfg)
             in assertBool
                  "all custom keys present"
                  ( all
                      (`elem` keys)
                      ["OFFSET", "DEPTH", "KEYWORD", "TITLE", "CONTEXT", "VERB", "LOCATOR"]
                  )
        ]
    ]
