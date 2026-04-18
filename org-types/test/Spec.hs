module Main where

import qualified BodyTest
import qualified ConfigTest
import qualified StampTest
import Test.Tasty
import qualified TimeConversionTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-types"
      [ TimeConversionTest.tests
      , BodyTest.tests
      , StampTest.tests
      , ConfigTest.tests
      ]
