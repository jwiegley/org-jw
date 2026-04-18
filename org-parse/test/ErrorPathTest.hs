{-# LANGUAGE OverloadedStrings #-}

module ErrorPathTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO"]
    , _closedKeywords = ["DONE"]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

-- Assert that parsing produces a failure whose message contains the
-- expected substring.
expectErrorContaining :: String -> ByteString -> Assertion
expectErrorContaining needle bs =
  case parseOrgFile parseConfig "t.org" bs of
    Right _ ->
      assertFailure
        ("expected parse failure containing " ++ show needle)
    Left (_, msg) ->
      assertBool
        ("expected message to contain " ++ show needle ++ ", got: " ++ msg)
        (needle `isInfixOf` msg)

-- Assert that parsing succeeds (used for "this variant is actually legal").
expectSuccess :: ByteString -> Assertion
expectSuccess bs = case parseOrgFile parseConfig "t.org" bs of
  Right _ -> pure ()
  Left (_, msg) -> assertFailure ("expected success, got error: " ++ msg)

tests :: TestTree
tests =
  testGroup
    "parser error paths"
    [ testGroup
        "parseTimeSingle range errors"
        [ testCase "year below 1970 is rejected" $
            expectErrorContaining
              "Year out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <1969-01-15 Wed>"])
        , testCase "year above 2200 is rejected" $
            expectErrorContaining
              "Year out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2201-01-15 Wed>"])
        , testCase "month 0 is rejected" $
            expectErrorContaining
              "Month out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-00-15 Mon>"])
        , testCase "month 13 is rejected" $
            expectErrorContaining
              "Month out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-13-15 Mon>"])
        , testCase "day 0 is rejected" $
            expectErrorContaining
              "Day out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-01-00 Mon>"])
        , testCase "Feb 30 yields gregorian error" $
            -- 30 is <= 31 so we land in fromGregorianValid's Nothing branch.
            expectErrorContaining
              "gregorian"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-02-30 Fri>"])
        , testCase "hour 24 is rejected" $
            expectErrorContaining
              "Hour out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-01-15 Mon 24:00>"])
        , testCase "minute 60 is rejected" $
            expectErrorContaining
              "Minute out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-01-15 Mon 09:60>"])
        , testCase "end hour 24 is rejected" $
            expectErrorContaining
              "Hour out of range"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-01-15 Mon 09:00-24:00>"])
        , testCase "end minute 99 is rejected" $
            expectErrorContaining
              "Minute out of range"
              ( fromLines
                  ["* TODO Task", "SCHEDULED: <2024-01-15 Mon 09:00-10:99>"]
              )
        ]
    , testGroup
        "parseTimeSingle suffix errors"
        [ testCase ".- unrecognized suffix kind" $
            expectErrorContaining
              "period"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-01-15 Mon .-1d>"])
        ]
    , testGroup
        "parseTime range composition errors"
        [ testCase "ending time with suffix is rejected" $
            expectErrorContaining
              "Invalid ending time (has suffix)"
              ( fromLines
                  [ "* TODO Task"
                  , "<2024-01-15 Mon>--<2024-01-20 Sat +1w>"
                  ]
              )
        ]
    , testGroup
        "trailing text at the end of a file"
        [ testCase "entries must terminate cleanly or produce an error" $
            expectSuccess (fromLines ["* TODO Task"])
        ]
    , testGroup
        "mid-file error edge cases"
        [ testCase "empty input still parses as OrgFile" $ do
            case parseOrgFile parseConfig "t.org" "" of
              Right (OrgFile _ _ es) -> es @?= []
              Left (_, msg) ->
                assertFailure ("expected success on empty input, got " ++ msg)
        ]
    ]
