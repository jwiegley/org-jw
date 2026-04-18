module PureLintTest (tests) where

import Org.Lint
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "pure helpers"
    [ testGroup
        "consistent"
        [ testCase "empty list is consistent" $
            consistent ([] :: [Int]) @?= True
        , testCase "singleton is consistent" $
            consistent [42 :: Int] @?= True
        , testCase "all equal is consistent" $
            consistent [1 :: Int, 1, 1] @?= True
        , testCase "mismatched is inconsistent" $
            consistent [1 :: Int, 1, 2] @?= False
        ]
    , testGroup
        "parseLintMessageKind"
        [ testCase "parses ERROR" $
            parseLintMessageKind "ERROR" @?= Just LintError
        , testCase "parses WARN" $
            parseLintMessageKind "WARN" @?= Just LintWarn
        , testCase "parses INFO" $
            parseLintMessageKind "INFO" @?= Just LintInfo
        , testCase "parses ALL" $
            parseLintMessageKind "ALL" @?= Just LintAll
        , testCase "parses DEBUG" $
            parseLintMessageKind "DEBUG" @?= Just LintDebug
        , testCase "rejects unknown" $
            parseLintMessageKind "bogus" @?= Nothing
        , testCase "is case-sensitive" $
            parseLintMessageKind "error" @?= Nothing
        ]
    ]
