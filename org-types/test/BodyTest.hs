{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module BodyTest (tests) where

import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "t.org" 0

ws :: String -> Block
ws = Whitespace loc0

para :: [String] -> Block
para = Paragraph loc0

tests :: TestTree
tests =
  testGroup
    "Body mempty / (<>) / emptyBody"
    [ testGroup
        "emptyBody"
        [ testCase "mempty is empty" $
            emptyBody mempty @?= True
        , testCase "Body [] is empty" $
            emptyBody (Body []) @?= True
        , testCase "Body with one ws block is not empty" $
            emptyBody (Body [ws ""]) @?= False
        , testCase "Body with one paragraph is not empty" $
            emptyBody (Body [para ["x"]]) @?= False
        ]
    , testGroup
        "Body mempty"
        [ testCase "mempty equals Body []" $
            (mempty :: Body) @?= Body []
        ]
    , testGroup
        "Body Semigroup"
        [ testCase "empty <> x = x" $
            mempty <> Body [para ["a"]] @?= Body [para ["a"]]
        , testCase "x <> empty = x" $
            Body [para ["a"]] <> mempty @?= Body [para ["a"]]
        , testCase "two paragraphs merge" $
            Body [para ["a"]] <> Body [para ["b"]]
              @?= Body [para ["a", "b"]]
        , testCase "whitespace merges with adjacent whitespace" $
            Body [ws "x"] <> Body [ws "y"] @?= Body [ws "xy"]
        , testCase "paragraph then whitespace-then-paragraph stays separate" $
            Body [para ["a"]] <> Body [ws "\n", para ["b"]]
              @?= Body [para ["a"], ws "\n", para ["b"]]
        , testCase "paragraph <> whitespace remains distinct" $
            Body [para ["a"]] <> Body [ws " "]
              @?= Body [para ["a"], ws " "]
        , testCase "whitespace <> paragraph remains distinct" $
            Body [ws " "] <> Body [para ["a"]]
              @?= Body [ws " ", para ["a"]]
        , testCase "mappend same as <>" $
            Body [ws "x"] `mappend` Body [ws "y"] @?= Body [ws "xy"]
        ]
    ]
