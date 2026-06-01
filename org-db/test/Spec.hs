module Main where

import qualified EmbedRetryTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-db"
      [ EmbedRetryTest.tests
      ]
