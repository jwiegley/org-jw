module FileNameTest (tests) where

import Org.Data (fileNameReTest)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "fileNameRe parses all 30 name shapes" fileNameReTest
