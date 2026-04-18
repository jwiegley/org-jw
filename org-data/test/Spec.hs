module Main where

import qualified FileNameTest
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
      ]
