module Main where

import qualified ShowDurationTest
import qualified ShowLogEntryExtraTest
import qualified ShowLogEntryTest
import qualified ShowOrgFileTest
import qualified ShowTimeExtraTest
import qualified ShowTimeTest
import qualified SummarizeEntryExtraTest
import qualified SummarizeEntryTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-print"
      [ ShowTimeTest.tests
      , ShowTimeExtraTest.tests
      , ShowDurationTest.tests
      , ShowLogEntryTest.tests
      , ShowLogEntryExtraTest.tests
      , ShowOrgFileTest.tests
      , SummarizeEntryTest.tests
      , SummarizeEntryExtraTest.tests
      ]
