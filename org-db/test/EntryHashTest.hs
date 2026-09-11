module EntryHashTest (tests) where

import Control.Lens ((&), (.~), _2)
import qualified Data.Text as T
import Org.DB.Store (computeEntryHash)
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

{- | A minimal but representative entry: keyword, tags, properties, stamps,
log entries (including a LogBook with a nested entry), a body with every
block kind, and a sub-entry. Every Loc in the tree is built from the same
@path@ and @pos@ parameters, so varying them varies every Loc at once.
-}
sampleEntry :: String -> Int -> Entry
sampleEntry path pos =
  Entry
    { _entryLoc = Loc path pos
    , _entryDepth = 1
    , _entryKeyword = Just (OpenKeyword (Loc path pos) "TODO")
    , _entryPriority = Just "A"
    , _entryHeadline = "Fix the pool leak"
    , _entryVerb = Nothing
    , _entryTitle = "Fix the pool leak"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = [PlainTag "pool", PlainTag "home"]
    , _entryStamps =
        [ ScheduledStamp
            (Loc path pos)
            Time
              { _timeKind = ActiveTime
              , _timeDay = 20000
              , _timeDayEnd = Nothing
              , _timeStart = Just 480
              , _timeEnd = Just 540
              , _timeSuffix = Just (TimeSuffix TimeRepeat 1 DaySpan Nothing)
              }
        ]
    , _entryProperties =
        [ Property (Loc path pos) False "ID" "1625F26A-188A-476E-8F2A-380910373AC7"
        , Property (Loc path pos) False "CATEGORY" "Pool"
        ]
    , _entryLogEntries =
        [ LogState
            (Loc path pos)
            (ClosedKeyword (Loc path pos) "DONE")
            (Just (OpenKeyword (Loc path pos) "TODO"))
            Time
              { _timeKind = ActiveTime
              , _timeDay = 20001
              , _timeDayEnd = Nothing
              , _timeStart = Just 600
              , _timeEnd = Nothing
              , _timeSuffix = Nothing
              }
            (Just (Body [Paragraph (Loc path pos) ["Closed after repair."]]))
        , LogBook
            (Loc path pos)
            [ LogClock
                (Loc path pos)
                Time
                  { _timeKind = ActiveTime
                  , _timeDay = 20002
                  , _timeDayEnd = Nothing
                  , _timeStart = Just 480
                  , _timeEnd = Just 510
                  , _timeSuffix = Nothing
                  }
                (Just (Duration 0 30))
            ]
        ]
    , _entryBody =
        Body
          [ Whitespace (Loc path pos) "\n"
          , Paragraph (Loc path pos) ["The pool loses", "about an inch a day."]
          , Drawer (Loc path pos) (PlainDrawer "LOGBOOK") ["CLOCK: [2002-01-01]"]
          , InlineTask (Loc path pos) (sampleEntry path (pos + 100))
          ]
    , _entryItems = []
    }

mkTime :: Integer -> Time
mkTime day =
  Time
    { _timeKind = ActiveTime
    , _timeDay = day
    , _timeDayEnd = Nothing
    , _timeStart = Nothing
    , _timeEnd = Nothing
    , _timeSuffix = Nothing
    }

tests :: TestTree
tests =
  testGroup
    "EntryHash"
    [ testCase "hash is stable when every Loc changes (path and position)" $ do
        let h1 = computeEntryHash (sampleEntry "/Users/johnw/org/pool.org" 400)
            h2 = computeEntryHash (sampleEntry "/some/other/file.org" 99999)
        h1 @?= h2
    , testCase "hash carries the scheme version marker" $ do
        T.pack "v2:" `T.isPrefixOf` computeEntryHash (sampleEntry "/x.org" 0) @? "hash should carry scheme version marker"
    , testCase "hash changes when actual content changes" $ do
        let base = sampleEntry "/f.org" 10
            a = computeEntryHash base
            changed =
              [ ("headline", base & entryHeadline .~ "Fix the roof leak")
              , ("body text", base & entryBody . blocks . traverse . _Paragraph . _2 .~ ["Different text entirely"])
              , ("stamps", base & entryStamps .~ [ClosedStamp (Loc "/f.org" 10) (mkTime 20003)])
              , ("log entries", base & entryLogEntries .~ [])
              , ("properties", base & entryProperties .~ [])
              , ("tags", base & entryTags .~ [])
              , ("keyword", base & entryKeyword .~ Nothing)
              , ("priority", base & entryPriority .~ Just "B")
              , ("title", base & entryTitle .~ "Fix the roof leak")
              ]
        mapM_
          (\(lbl, e) -> (computeEntryHash e /= a) @? (lbl ++ " change must alter the hash"))
          changed
    ]
