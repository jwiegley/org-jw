{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Main where

-- import Hedgehog hiding (Action)
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Test.Tasty

-- import Test.Tasty.HUnit
-- import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "org-data"
      []
