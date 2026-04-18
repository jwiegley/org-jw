module Main where

import qualified EntryParseTest
import qualified ErrorPathTest
import qualified FilePropertiesTest
import qualified LogEntryParseTest
import qualified ParseUncoveredTest
import qualified StampParseTest
import Test.Tasty
import qualified TimeParseTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-parse"
      [ TimeParseTest.tests
      , StampParseTest.tests
      , LogEntryParseTest.tests
      , ErrorPathTest.tests
      , FilePropertiesTest.tests
      , EntryParseTest.tests
      , ParseUncoveredTest.tests
      ]
