module ConfigTest (tests) where

import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Config defaults"
    [ testCase "default startKeywords is empty" $
        _startKeywords defaultConfig @?= []
    , testCase "default openKeywords is empty" $
        _openKeywords defaultConfig @?= []
    , testCase "default closedKeywords is empty" $
        _closedKeywords defaultConfig @?= []
    , testCase "default keywordTransitions is empty" $
        _keywordTransitions defaultConfig @?= []
    , testCase "default homeDirectory is Nothing" $
        _homeDirectory defaultConfig @?= Nothing
    , testCase "default checkFiles is True" $
        _checkFiles defaultConfig @?= True
    , testCase "default priorities is empty" $
        _priorities defaultConfig @?= []
    , testCase "default propertyColumn is 0" $
        _propertyColumn defaultConfig @?= 0
    , testCase "default tagsColumn is 0" $
        _tagsColumn defaultConfig @?= 0
    , testCase "default attachmentsDir is empty" $
        _attachmentsDir defaultConfig @?= ""
    ]
