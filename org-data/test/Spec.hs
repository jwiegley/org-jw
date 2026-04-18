module Main where

import qualified CollectionMapTest
import qualified DataHelpersTest
import qualified FileNameTest
import qualified PropertyLookupTest
import qualified RoundTripTest
import qualified SluggifyTest
import qualified TagListTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-data"
      [ FileNameTest.tests
      , SluggifyTest.tests
      , TagListTest.tests
      , RoundTripTest.tests
      , DataHelpersTest.tests
      , PropertyLookupTest.tests
      , CollectionMapTest.tests
      ]
