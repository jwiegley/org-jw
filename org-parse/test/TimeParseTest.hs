{-# LANGUAGE OverloadedStrings #-}

module TimeParseTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- A minimal config adequate for the parser itself. The parser only consults
-- the keyword lists; everything else is irrelevant.
parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

-- Parse a single entry whose stamps exercise the parseTimeSingle / parseTime
-- paths. The entry header is the minimum legal fixture.
parse :: ByteString -> Either String OrgFile
parse bs =
  either (Left . snd) Right (parseOrgFile parseConfig "t.org" bs)

-- Extract the single entry in a one-entry file.
singleEntry :: ByteString -> Either String Entry
singleEntry bs = case parse bs of
  Right (OrgFile _ _ [e]) -> Right e
  Right (OrgFile _ _ _) -> Left "expected exactly one entry"
  Left msg -> Left msg

-- Pull the first stamp from an entry for inspection.
firstStamp :: Entry -> Maybe Stamp
firstStamp e = case _entryStamps e of
  (s : _) -> Just s
  [] -> Nothing

-- Pull the time out of any stamp.
stampTime :: Stamp -> Time
stampTime (ClosedStamp _ t) = t
stampTime (ScheduledStamp _ t) = t
stampTime (DeadlineStamp _ t) = t
stampTime (ActiveStamp _ t) = t

tests :: TestTree
tests =
  testGroup
    "parseTime / parseTimeSingle"
    [ testGroup
        "active vs inactive kinds"
        [ testCase "active timestamp in SCHEDULED" $ do
            e <- eitherFail $ singleEntry activeSchedule
            case firstStamp e of
              Just (ScheduledStamp _ t) -> _timeKind t @?= ActiveTime
              other -> assertFailure ("expected ScheduledStamp, got " ++ show other)
        , testCase "inactive timestamp in SCHEDULED" $ do
            e <- eitherFail $ singleEntry inactiveSchedule
            case firstStamp e of
              Just (ScheduledStamp _ t) -> _timeKind t @?= InactiveTime
              other -> assertFailure ("expected ScheduledStamp, got " ++ show other)
        ]
    , testGroup
        "time-of-day variants"
        [ testCase "start time only" $ do
            e <- eitherFail $ singleEntry startOnly
            let t = stampTime <$> firstStamp e
            fmap _timeStart t @?= Just (Just (9 * 60))
            fmap _timeEnd t @?= Just Nothing
        , testCase "start and end on same day" $ do
            e <- eitherFail $ singleEntry rangeSameDay
            let t = stampTime <$> firstStamp e
            fmap _timeStart t @?= Just (Just (9 * 60))
            fmap _timeEnd t @?= Just (Just (17 * 60))
        ]
    , testGroup
        "date ranges via --"
        [ testCase "active range across days blends via parseTime" $ do
            -- Active stamp on entry body uses parseTime (not parseTimeSingle),
            -- and so --blends start/end day.
            e <- eitherFail $ singleEntry activeRangeAcrossDays
            case _entryStamps e of
              [ActiveStamp _ t] -> do
                _timeKind t @?= ActiveTime
                assertBool "has dayEnd" (case _timeDayEnd t of Just _ -> True; _ -> False)
              other -> assertFailure ("expected one ActiveStamp, got " ++ show other)
        ]
    , testGroup
        "time suffixes"
        [ testCase "+1w weekly repeat (TimeRepeat)" $ do
            e <- eitherFail $ singleEntry suffixRepeat
            sfx <- expectSuffix e
            _suffixKind sfx @?= TimeRepeat
            _suffixSpan sfx @?= WeekSpan
            _suffixNum sfx @?= 1
        , testCase "++1m monthly TimeRepeatPlus" $ do
            e <- eitherFail $ singleEntry suffixRepeatPlus
            sfx <- expectSuffix e
            _suffixKind sfx @?= TimeRepeatPlus
        , testCase ".+1d TimeDottedRepeat" $ do
            e <- eitherFail $ singleEntry suffixDottedRepeat
            sfx <- expectSuffix e
            _suffixKind sfx @?= TimeDottedRepeat
        , testCase "-2d TimeWithin" $ do
            e <- eitherFail $ singleEntry suffixWithin
            sfx <- expectSuffix e
            _suffixKind sfx @?= TimeWithin
        , testCase "YearSpan suffix" $ do
            e <- eitherFail $ singleEntry suffixYear
            sfx <- expectSuffix e
            _suffixSpan sfx @?= YearSpan
        , testCase "MonthSpan suffix" $ do
            e <- eitherFail $ singleEntry suffixMonth
            sfx <- expectSuffix e
            _suffixSpan sfx @?= MonthSpan
        , testCase "DaySpan suffix" $ do
            e <- eitherFail $ singleEntry suffixDay
            sfx <- expectSuffix e
            _suffixSpan sfx @?= DaySpan
        , testCase "larger span with /3w" $ do
            e <- eitherFail $ singleEntry suffixLargerSpanW
            sfx <- expectSuffix e
            _suffixLargerSpan sfx @?= Just (3, WeekSpan)
        , testCase "larger span with /3d" $ do
            e <- eitherFail $ singleEntry suffixLargerSpanD
            sfx <- expectSuffix e
            _suffixLargerSpan sfx @?= Just (3, DaySpan)
        , testCase "larger span with /3m" $ do
            e <- eitherFail $ singleEntry suffixLargerSpanM
            sfx <- expectSuffix e
            _suffixLargerSpan sfx @?= Just (3, MonthSpan)
        , testCase "larger span with /3y" $ do
            e <- eitherFail $ singleEntry suffixLargerSpanY
            sfx <- expectSuffix e
            _suffixLargerSpan sfx @?= Just (3, YearSpan)
        ]
    , testGroup
        "day of week strings cover all 7"
        [ testCase "Sun" $ assertParses scheduleDow "Sun"
        , testCase "Mon" $ assertParses scheduleDow "Mon"
        , testCase "Tue" $ assertParses scheduleDow "Tue"
        , testCase "Wed" $ assertParses scheduleDow "Wed"
        , testCase "Thu" $ assertParses scheduleDow "Thu"
        , testCase "Fri" $ assertParses scheduleDow "Fri"
        , testCase "Sat" $ assertParses scheduleDow "Sat"
        ]
    ]

-- Helper: crash the test with a nice message on a Left.
eitherFail :: Either String a -> IO a
eitherFail = either assertFailure pure

-- Helper: assert the first stamp in an entry has a suffix, returning it.
expectSuffix :: Entry -> IO TimeSuffix
expectSuffix e = case firstStamp e of
  Just s -> case _timeSuffix (stampTime s) of
    Just sfx -> pure sfx
    Nothing -> assertFailure "expected a time suffix"
  Nothing -> assertFailure "expected at least one stamp"

-- Assert that a fixture parser driver produces a valid file.
assertParses :: (ByteString -> ByteString) -> ByteString -> Assertion
assertParses mk dow =
  case parse (mk dow) of
    Right _ -> pure ()
    Left msg ->
      assertFailure ("expected parse success for " ++ show dow ++ ": " ++ msg)

-- Build a fixture that varies only in the day-of-week string.
scheduleDow :: ByteString -> ByteString
scheduleDow dow =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-01 " <> dow <> ">"
    ]

activeSchedule :: ByteString
activeSchedule =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon>"
    ]

inactiveSchedule :: ByteString
inactiveSchedule =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: [2024-01-15 Mon]"
    ]

startOnly :: ByteString
startOnly =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon 09:00>"
    ]

rangeSameDay :: ByteString
rangeSameDay =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon 09:00-17:00>"
    ]

activeRangeAcrossDays :: ByteString
activeRangeAcrossDays =
  fromLines
    [ "* TODO Task"
    , "<2024-01-15 Mon>--<2024-01-16 Tue>"
    ]

suffixRepeat :: ByteString
suffixRepeat =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1w>"
    ]

suffixRepeatPlus :: ByteString
suffixRepeatPlus =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon ++1m>"
    ]

suffixDottedRepeat :: ByteString
suffixDottedRepeat =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon .+1d>"
    ]

suffixWithin :: ByteString
suffixWithin =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon -2d>"
    ]

suffixYear :: ByteString
suffixYear =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1y>"
    ]

suffixMonth :: ByteString
suffixMonth =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1m>"
    ]

suffixDay :: ByteString
suffixDay =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1d>"
    ]

suffixLargerSpanW :: ByteString
suffixLargerSpanW =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1d/3w>"
    ]

suffixLargerSpanD :: ByteString
suffixLargerSpanD =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1d/3d>"
    ]

suffixLargerSpanM :: ByteString
suffixLargerSpanM =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1d/3m>"
    ]

suffixLargerSpanY :: ByteString
suffixLargerSpanY =
  fromLines
    [ "* TODO Task"
    , "SCHEDULED: <2024-01-15 Mon +1d/3y>"
    ]
