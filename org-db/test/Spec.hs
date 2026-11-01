module Main where

import qualified EmbedRetryTest
import qualified EntryHashTest
import qualified RenderTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-db"
      [ EmbedRetryTest.tests
      , EntryHashTest.tests
      , RenderTest.tests
      ]
