{-# LANGUAGE OverloadedStrings #-}

module StringTimeTest (tests) where

import Control.Lens (review, (&), (.~), (^.), (^..), (^?))
import Data.Maybe (isJust)
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "s.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkFile :: FilePath -> OrgFile
mkFile p =
  OrgFile
    { _orgFilePath = p
    , _orgFileHeader =
        Header
          { _headerPropertiesDrawer = []
          , _headerFileProperties = []
          , _headerPreamble = emptyBody0
          }
    , _orgFileEntries = []
    }

tests :: TestTree
tests =
  testGroup
    "stringTime, _Time prism, and dirName"
    [ testGroup
        "stringTime traversal"
        [ testCase "parses JustDay format" $
            let result = "20240601" ^? stringTime
             in isJust result @?= True
        , testCase "parses JustDay format - zero time start" $
            let result = "20240601" ^? stringTime
             in (result >>= _timeStart) @?= Nothing
        , testCase "parses HourMin format" $
            let result = "202406011430" ^? stringTime
             in isJust result @?= True
        , testCase "parses HourMin sets timeStart" $
            let result = "202406010930" ^? stringTime
             in isJust (result >>= _timeStart) @?= True
        , testCase "parses HourMinSec format" $
            let result = "20240601143015" ^? stringTime
             in isJust result @?= True
        , testCase "unparsable string yields no result" $
            ("not-a-timestamp" ^? stringTime) @?= Nothing
        , testCase "empty string yields no result" $
            ("" ^? stringTime) @?= Nothing
        , testCase "many results is one or zero" $
            length ("20240601" ^.. stringTime) @?= 1
        , testCase "re-serializes round trip" $
            let updated = "20240601" & stringTime .~ mkTime 60000 Nothing
             in isJust (updated ^? stringTime) @?= True
        ]
    , testGroup
        "_Time prism"
        [ testCase "review builds a string" $
            let tm = mkTime 60000 Nothing
                s = review _Time tm
             in assertBool "non-empty string" (not (null s))
        , testCase "preview fails on unparsable string" $
            ("xyz" ^? _Time) @?= Nothing
        , testCase "round trip through _Time" $
            let tm = mkTime 60000 Nothing
                s = review _Time tm
                back = s ^? _Time
             in isJust back @?= True
        ]
    , testGroup
        "dirName"
        [ testCase "gets directory" $
            "/a/b/c.org" ^. dirName @?= "/a/b"
        , testCase "gets dot for bare filename" $
            "c.org" ^. dirName @?= "."
        , testCase "sets directory preserves filename" $
            ("/a/b/c.org" & dirName .~ "/x/y") @?= "/x/y/c.org"
        , testCase "sets dot preserves filename" $
            ("c.org" & dirName .~ "/somewhere") @?= "/somewhere/c.org"
        ]
    , testGroup
        "fileName lens"
        [ testCase "gets file name" $
            "/a/b/c.org" ^. fileName @?= "c.org"
        , testCase "sets file name preserves directory" $
            ("/a/b/c.org" & fileName .~ "new.org") @?= "/a/b/new.org"
        , testCase "filename only" $
            "only.org" ^. fileName @?= "only.org"
        ]
    , testGroup
        "fileTimestamp on CollectionItems"
        [ testCase "OrgItem with bare-date filename has a timestamp" $
            let item = OrgItem (mkFile "20240601")
             in (item ^? fileTimestamp) /= Nothing @?= True
        , testCase "OrgItem with non-date filename has no timestamp" $
            let item = OrgItem (mkFile "topic.org")
             in item ^? fileTimestamp @?= Nothing
        , testCase "DataItem with bare-date filename has a timestamp" $
            let item = DataItem "20240601"
             in (item ^? fileTimestamp) /= Nothing @?= True
        , testCase "DataItem with non-date filename has no timestamp" $
            let item = DataItem "image.png"
             in item ^? fileTimestamp @?= Nothing
        ]
    , testGroup
        "fileCreatedTime"
        [ testCase "uses CREATED property when present" $
            let props = [Property loc0 False "CREATED" "[2024-06-01 Sat]"]
                base = mkFile "no-date.org"
                f =
                  base
                    { _orgFileHeader =
                        (_orgFileHeader base)
                          { _headerFileProperties = props
                          }
                    }
             in (OrgItem f ^? fileCreatedTime) /= Nothing @?= True
        , testCase "falls back to filename timestamp for bare date" $
            let item = OrgItem (mkFile "20240601")
             in (item ^? fileCreatedTime) /= Nothing @?= True
        , testCase "no CREATED and no date in filename yields Nothing" $
            let item = OrgItem (mkFile "plain.org")
             in item ^? fileCreatedTime @?= Nothing
        ]
    ]
 where
  mkTime :: Integer -> Maybe Integer -> Time
  mkTime day startM =
    Time
      { _timeKind = InactiveTime
      , _timeDay = day
      , _timeDayEnd = Nothing
      , _timeStart = startM
      , _timeEnd = Nothing
      , _timeSuffix = Nothing
      }
