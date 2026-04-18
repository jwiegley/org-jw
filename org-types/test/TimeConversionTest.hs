module TimeConversionTest (tests) where

import Data.Time (Day (ModifiedJulianDay), UTCTime (..), secondsToDiffTime)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

midnightActive :: Time
midnightActive = Time ActiveTime 60000 Nothing Nothing Nothing Nothing

oneAM :: Time
oneAM = midnightActive{_timeStart = Just 60}

sameDayRange :: Time
sameDayRange =
  Time
    ActiveTime
    60000
    (Just 60000)
    (Just (9 * 60))
    (Just (17 * 60))
    Nothing

crossDayRange :: Time
crossDayRange =
  Time
    ActiveTime
    60000
    (Just 60001)
    (Just (23 * 60 + 30))
    (Just (1 * 60 + 30))
    Nothing

tests :: TestTree
tests =
  testGroup
    "Time conversions"
    [ testGroup
        "timeStartToUTCTime"
        [ testCase "midnight produces UTC midnight" $
            timeStartToUTCTime midnightActive
              @?= UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0)
        , testCase "01:00 produces 3600 seconds" $
            timeStartToUTCTime oneAM
              @?= UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 3600)
        , testCase "no start means 0 seconds" $
            timeStartToUTCTime (midnightActive{_timeStart = Nothing})
              @?= UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0)
        ]
    , testGroup
        "timeEndToUTCTime"
        [ testCase "returns Nothing when no end day" $
            timeEndToUTCTime midnightActive @?= Nothing
        , testCase "returns end UTC when end day set, same day" $
            timeEndToUTCTime sameDayRange
              @?= Just
                ( UTCTime
                    (ModifiedJulianDay 60000)
                    (secondsToDiffTime (17 * 3600))
                )
        , testCase "uses end day when different from start" $
            timeEndToUTCTime crossDayRange
              @?= Just
                ( UTCTime
                    (ModifiedJulianDay 60001)
                    (secondsToDiffTime (90 * 60))
                )
        , testCase "uses 0 when end-time missing but end-day set" $
            timeEndToUTCTime
              (midnightActive{_timeDayEnd = Just 60001, _timeEnd = Nothing})
              @?= Just
                (UTCTime (ModifiedJulianDay 60001) (secondsToDiffTime 0))
        ]
    , testGroup
        "utcTimeToTime"
        [ testCase "preserves day number" $
            _timeDay
              ( utcTimeToTime
                  ActiveTime
                  (UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0))
              )
              @?= 60000
        , testCase "stores kind" $
            _timeKind
              ( utcTimeToTime
                  InactiveTime
                  (UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0))
              )
              @?= InactiveTime
        , testCase "converts seconds to minute quantity" $
            _timeStart
              ( utcTimeToTime
                  ActiveTime
                  (UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 3600))
              )
              @?= Just 60
        , testCase "clears dayEnd, end, suffix" $
            let t =
                  utcTimeToTime
                    ActiveTime
                    (UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0))
             in (_timeDayEnd t, _timeEnd t, _timeSuffix t)
                  @?= (Nothing, Nothing, Nothing)
        ]
    , testGroup
        "roundtrip via UTCTime"
        [ testCase "utcTimeToTime . timeStartToUTCTime preserves day + start" $
            let tm = oneAM
                u = timeStartToUTCTime tm
                tm' = utcTimeToTime ActiveTime u
             in (_timeDay tm', _timeStart tm')
                  @?= (_timeDay tm, _timeStart tm)
        , testCase "conversion through InactiveTime kind" $
            let u =
                  UTCTime
                    (ModifiedJulianDay 60000)
                    (secondsToDiffTime (2 * 3600))
                t = utcTimeToTime InactiveTime u
             in (_timeKind t, _timeStart t) @?= (InactiveTime, Just 120)
        ]
    , testGroup
        "Ord Time"
        [ testCase "earlier start compares less" $
            compare midnightActive oneAM @?= LT
        , testCase "equal times compare EQ" $
            compare midnightActive midnightActive @?= EQ
        , testCase "later start compares greater" $
            compare oneAM midnightActive @?= GT
        , testCase "different days compare" $
            compare midnightActive (midnightActive{_timeDay = 60001})
              @?= LT
        ]
    ]
