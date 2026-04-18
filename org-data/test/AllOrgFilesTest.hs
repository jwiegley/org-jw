{-# LANGUAGE OverloadedStrings #-}

module AllOrgFilesTest (tests) where

import Control.Lens (toListOf, (^..))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "x.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkFile :: FilePath -> [Property] -> OrgFile
mkFile p props =
  OrgFile
    { _orgFilePath = p
    , _orgFileHeader =
        Header
          { _headerPropertiesDrawer = []
          , _headerFileProperties = props
          , _headerPreamble = emptyBody0
          }
    , _orgFileEntries = []
    }

-- File name with embedded [tag1 tag2] is parsed via fileNameTags.
plainFile :: OrgFile
plainFile = mkFile "notes.org" []

taggedFile :: OrgFile
taggedFile = mkFile "20240601-thing[work urgent].org" []

-- OrgFile with an explicit #+filetags: property; the tag list is read
-- from that property rather than from the filename.
filetagsFile :: OrgFile
filetagsFile =
  mkFile
    "misc.org"
    [Property loc0 False "filetags" "::home:errands::"]

sampleColl :: Collection
sampleColl =
  Collection
    [ OrgItem plainFile
    , DataItem "image.png"
    , OrgItem taggedFile
    , OrgItem filetagsFile
    ]

tests :: TestTree
tests =
  testGroup
    "allOrgFiles & allTaggedItems"
    [ testGroup
        "allOrgFiles"
        [ testCase "collects only OrgItem entries" $
            length (toListOf allOrgFiles sampleColl) @?= 3
        , testCase "preserves OrgFile contents" $
            let files = toListOf allOrgFiles sampleColl
             in map _orgFilePath files
                  @?= [ "notes.org"
                      , "20240601-thing[work urgent].org"
                      , "misc.org"
                      ]
        , testCase "empty sampleColl yields no files" $
            toListOf allOrgFiles (Collection []) @?= []
        , testCase "sampleColl with only DataItems yields empty" $
            toListOf allOrgFiles (Collection [DataItem "a", DataItem "b"])
              @?= []
        ]
    , testGroup
        "allTaggedItems"
        [ testCase "yields a pair for each item" $
            length (toListOf allTaggedItems sampleColl) @?= 4
        , testCase "OrgItem without filetags uses empty tag list" $
            let pairs = sampleColl ^.. allTaggedItems
             in lookup "notes.org" pairs @?= Just []
        , testCase "OrgItem with filetags property picks up tags" $
            let pairs = sampleColl ^.. allTaggedItems
             in lookup "misc.org" pairs
                  @?= Just [PlainTag "home", PlainTag "errands"]
        , testCase "OrgItem with filename tags picks up tags" $
            let pairs = sampleColl ^.. allTaggedItems
             in lookup "20240601-thing[work urgent].org" pairs
                  @?= Just [PlainTag "work", PlainTag "urgent"]
        , testCase "DataItem path is included with (possibly) empty tags" $
            let pairs = sampleColl ^.. allTaggedItems
             in lookup "image.png" pairs @?= Just []
        , testCase "empty sampleColl yields no pairs" $
            toListOf allTaggedItems (Collection [])
              @?= ([] :: [(FilePath, [Tag])])
        ]
    ]
