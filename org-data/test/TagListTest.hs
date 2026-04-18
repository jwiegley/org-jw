module TagListTest (tests) where

import Control.Lens (from, (^.))
import Org.Data (tagList)
import Org.Types (Tag (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "tagList iso"
    [ testCase "empty list renders as :::" $
        ([] :: [Tag]) ^. tagList @?= ":::"
    , testCase "forward encodes tags with : delimiters" $
        [PlainTag "foo", PlainTag "bar"] ^. tagList @?= "::foo:bar::"
    , testCase "backward parses delimited string" $
        ":foo:bar:" ^. from tagList @?= [PlainTag "foo", PlainTag "bar"]
    , testCase "roundtrip is identity" $
        let xs = [PlainTag "a", PlainTag "b", PlainTag "c"]
         in xs ^. (tagList . from tagList) @?= xs
    , testCase "backward drops empty segments" $
        ":::" ^. from tagList @?= ([] :: [Tag])
    ]
