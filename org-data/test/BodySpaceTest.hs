{-# LANGUAGE OverloadedStrings #-}

module BodySpaceTest (tests) where

import Control.Lens ((&), (.~), (^..), (^?))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "b.org" 0

wsBlock :: String -> Block
wsBlock = Whitespace loc0

para :: [String] -> Block
para = Paragraph loc0

tests :: TestTree
tests =
  testGroup
    "body space traversals"
    [ testGroup
        "leadSpace"
        [ testCase "reads leading whitespace when first block is Whitespace" $
            Body [wsBlock "   ", para ["text"]] ^? leadSpace @?= Just "   "
        , testCase "returns Nothing when first block is not Whitespace" $
            Body [para ["text"], wsBlock "  "] ^? leadSpace @?= Nothing
        , testCase "empty body returns Nothing" $
            Body [] ^? leadSpace @?= Nothing
        , testCase "sets leading whitespace" $
            (Body [wsBlock " ", para ["x"]] & leadSpace .~ "\t\t")
              ^? leadSpace
              @?= Just "\t\t"
        , testCase "setting leadSpace on non-whitespace head is a no-op" $
            let original = Body [para ["y"]]
                updated = original & leadSpace .~ "!!"
             in updated @?= original
        ]
    , testGroup
        "endSpace"
        [ testCase "reads trailing whitespace" $
            Body [para ["text"], wsBlock "\n\n"] ^? endSpace @?= Just "\n\n"
        , testCase "returns Nothing when last block is not whitespace" $
            Body [wsBlock "  ", para ["tail"]] ^? endSpace @?= Nothing
        , testCase "empty body returns Nothing" $
            Body [] ^? endSpace @?= Nothing
        , testCase "sets trailing whitespace" $
            (Body [para ["x"], wsBlock "\n"] & endSpace .~ "   ")
              ^? endSpace
              @?= Just "   "
        , testCase "single whitespace block is both lead and end" $
            let b = Body [wsBlock "  "]
             in (b ^? leadSpace, b ^? endSpace) @?= (Just "  ", Just "  ")
        ]
    , testGroup
        "lined traversal"
        [ testCase "reads joined lines" $
            ["one", "two"] ^.. lined @?= ["one\ntwo\n"]
        , testCase "transforms joined text" $
            (["a", "b"] & lined .~ "x\ny\n") @?= ["x", "y"]
        , testCase "empty list yields one empty joined string" $
            ([] :: [String]) ^.. lined @?= [""]
        , testCase "empty string sets to empty list" $
            (["a"] & lined .~ "") @?= ([] :: [String])
        , testCase "single line round-trips" $
            ["hello"] ^.. lined @?= ["hello\n"]
        ]
    ]
