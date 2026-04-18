module Main where

import qualified LintCheckersTest
import qualified LintExtraTest
import qualified LintRulesTest
import qualified LintUncoveredTest
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
      , LintRulesTest.tests
      , LintCheckersTest.tests
      , LintUncoveredTest.tests
      , LintExtraTest.tests
      ]
