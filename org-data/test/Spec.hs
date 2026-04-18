module Main where

import qualified AllOrgFilesTest
import qualified AnyPropertyTest
import qualified BodySpaceTest
import qualified CollectionMapTest
import qualified DataHelpersTest
import qualified FieldLensesTest
import qualified FileNameTest
import qualified FoldEntriesTest
import qualified MoreHelpersTest
import qualified PlatedTest
import qualified PrismTest
import qualified PropertyLookupTest
import qualified RoundTripTest
import qualified SluggifyTest
import qualified StampTimeTest
import qualified StringTimeTest
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
      , AllOrgFilesTest.tests
      , AnyPropertyTest.tests
      , BodySpaceTest.tests
      , FoldEntriesTest.tests
      , PrismTest.tests
      , StampTimeTest.tests
      , StringTimeTest.tests
      , FieldLensesTest.tests
      , PlatedTest.tests
      , MoreHelpersTest.tests
      ]
