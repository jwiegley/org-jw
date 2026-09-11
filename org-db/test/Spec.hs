module Main where

import qualified EmbedRetryTest
import qualified EntryHashTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-db"
      [ EmbedRetryTest.tests
      , EntryHashTest.tests
      ]
