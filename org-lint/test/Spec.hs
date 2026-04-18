module Main where

import qualified PureLintTest
import qualified ShowLintOrgTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-lint"
      [ PureLintTest.tests
      , ShowLintOrgTest.tests
      ]
