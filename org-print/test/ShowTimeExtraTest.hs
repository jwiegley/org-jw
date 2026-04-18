module ShowTimeExtraTest (tests) where

import Control.Monad.Reader (runReader)
import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

day0 :: Integer
day0 = 60000

baseTm :: Time
baseTm = Time ActiveTime day0 Nothing Nothing Nothing Nothing

loc0 :: Loc
loc0 = Loc "t.org" 0

tm :: Time
tm = Time InactiveTime day0 Nothing Nothing Nothing Nothing

cfg :: Config
cfg = defaultConfig

entryWithLogs :: [LogEntry] -> Entry
entryWithLogs les =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "x"
    , _entryVerb = Nothing
    , _entryTitle = "x"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = les
    , _entryBody = Body []
    , _entryItems = []
    }

render :: [LogEntry] -> [String]
render les = runReader (showEntry (entryWithLogs les)) cfg

forceLines :: [String] -> [String]
forceLines = map (\s -> length s `seq` s)

clockRangeSameDay :: Time
clockRangeSameDay =
  baseTm
    { _timeDayEnd = Just day0
    , _timeStart = Just (9 * 60)
    , _timeEnd = Just (10 * 60)
    }

clockRangeCrossDay :: Time
clockRangeCrossDay =
  baseTm
    { _timeDayEnd = Just (day0 + 1)
    , _timeStart = Just (23 * 60)
    , _timeEnd = Just (1 * 60)
    }

tests :: TestTree
tests =
  testGroup
    "showTime extras"
    [ testCase "showTime only-day range inactive" $
        showTime (tm{_timeDayEnd = Just (day0 + 2)})
          @?= "[2023-02-25 Sat]--[2023-02-27 Mon]"
    , testCase "LogClock with duration emits split-time form same-day" $
        let out =
              forceLines
                (render [LogClock loc0 clockRangeSameDay (Just (Duration 1 0))])
            expected = "CLOCK: <2023-02-25 Sat 09:00>--<2023-02-25 Sat 10:00> =>  1:00"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogClock with duration emits split-time form cross-day" $
        let out =
              forceLines
                (render [LogClock loc0 clockRangeCrossDay (Just (Duration 2 0))])
            expected = "CLOCK: <2023-02-25 Sat 23:00>--<2023-02-26 Sun 01:00> =>  2:00"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogClock without duration uses showTimeSingle (forces internals)" $
        let tmStart = tm{_timeStart = Just (9 * 60)}
            out = forceLines (render [LogClock loc0 tmStart Nothing])
            expected = "CLOCK: [2023-02-25 Sat 09:00]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogClock without duration, end time ignored in single form" $
        -- When _timeDayEnd == _timeDay, showTimeSingle includes end time
        let tmBoth =
              tm
                { _timeDayEnd = Just day0
                , _timeStart = Just (9 * 60)
                , _timeEnd = Just (10 * 60)
                }
            out = forceLines (render [LogClock loc0 tmBoth Nothing])
            expected = "CLOCK: [2023-02-25 Sat 09:00-10:00]"
         in assertBool (show out) (expected `elem` out)
    , testCase "showTime full equality with start time" $
        showTime (baseTm{_timeStart = Just (14 * 60 + 30)})
          @?= "<2023-02-25 Sat 14:30>"
    , testCase "showTime full equality cross-day with end time" $
        showTime
          ( baseTm
              { _timeDayEnd = Just (day0 + 1)
              , _timeStart = Just (22 * 60)
              , _timeEnd = Just (2 * 60)
              }
          )
          @?= "<2023-02-25 Sat 22:00>--<2023-02-26 Sun 02:00>"
    ]
