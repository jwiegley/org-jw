module Main where

import qualified CombinatorsTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "flatparse-util"
      [ CombinatorsTest.tests
      ]
