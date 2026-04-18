module ShowTimeTest (tests) where

import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

day0 :: Integer
day0 = 60000

baseTm :: Time
baseTm = Time ActiveTime day0 Nothing Nothing Nothing Nothing

inactiveTm :: Time
inactiveTm = baseTm{_timeKind = InactiveTime}

tests :: TestTree
tests =
  testGroup
    "showTime"
    [ testGroup
        "basic"
        [ testCase "active day only" $
            showTime baseTm @?= "<2023-02-25 Sat>"
        , testCase "inactive day only" $
            showTime inactiveTm @?= "[2023-02-25 Sat]"
        , testCase "active with start minutes" $
            showTime (baseTm{_timeStart = Just (9 * 60)})
              @?= "<2023-02-25 Sat 09:00>"
        , testCase "active start + end on same day" $
            showTime
              ( baseTm
                  { _timeStart = Just (9 * 60)
                  , _timeEnd = Just (17 * 60)
                  }
              )
              @?= "<2023-02-25 Sat 09:00-17:00>"
        , testCase "active range across days" $
            showTime
              ( baseTm
                  { _timeDayEnd = Just (day0 + 1)
                  , _timeStart = Just (23 * 60)
                  , _timeEnd = Just (1 * 60)
                  }
              )
              @?= "<2023-02-25 Sat 23:00>--<2023-02-26 Sun 01:00>"
        , testCase "same dayEnd as day renders single stamp with end time" $
            showTime
              ( baseTm
                  { _timeDayEnd = Just day0
                  , _timeStart = Just (9 * 60)
                  , _timeEnd = Just (10 * 60)
                  }
              )
              @?= "<2023-02-25 Sat 09:00-10:00>"
        ]
    , testGroup
        "time suffixes"
        [ testCase "+1w weekly repeat" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just (TimeSuffix TimeRepeat 1 WeekSpan Nothing)
                  }
              )
              @?= "<2023-02-25 Sat +1w>"
        , testCase "++1m monthly repeat-plus" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just (TimeSuffix TimeRepeatPlus 1 MonthSpan Nothing)
                  }
              )
              @?= "<2023-02-25 Sat ++1m>"
        , testCase ".+1d dotted repeat" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just (TimeSuffix TimeDottedRepeat 1 DaySpan Nothing)
                  }
              )
              @?= "<2023-02-25 Sat .+1d>"
        , testCase "-2d within" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just (TimeSuffix TimeWithin 2 DaySpan Nothing)
                  }
              )
              @?= "<2023-02-25 Sat -2d>"
        , testCase "+1y year repeat" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just (TimeSuffix TimeRepeat 1 YearSpan Nothing)
                  }
              )
              @?= "<2023-02-25 Sat +1y>"
        , testCase "+1d/3w with larger week span" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just
                        ( TimeSuffix
                            TimeRepeat
                            1
                            DaySpan
                            (Just (3, WeekSpan))
                        )
                  }
              )
              @?= "<2023-02-25 Sat +1d/3w>"
        , testCase "+1d/3d larger day span" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just
                        ( TimeSuffix
                            TimeRepeat
                            1
                            DaySpan
                            (Just (3, DaySpan))
                        )
                  }
              )
              @?= "<2023-02-25 Sat +1d/3d>"
        , testCase "+1d/3m larger month span" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just
                        ( TimeSuffix
                            TimeRepeat
                            1
                            DaySpan
                            (Just (3, MonthSpan))
                        )
                  }
              )
              @?= "<2023-02-25 Sat +1d/3m>"
        , testCase "+1d/3y larger year span" $
            showTime
              ( baseTm
                  { _timeSuffix =
                      Just
                        ( TimeSuffix
                            TimeRepeat
                            1
                            DaySpan
                            (Just (3, YearSpan))
                        )
                  }
              )
              @?= "<2023-02-25 Sat +1d/3y>"
        ]
    ]
