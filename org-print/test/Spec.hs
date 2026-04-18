module Main where

import qualified ShowLogEntryTest
import qualified ShowTimeTest
import qualified SummarizeEntryTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-print"
      [ ShowTimeTest.tests
      , ShowLogEntryTest.tests
      , SummarizeEntryTest.tests
      ]
