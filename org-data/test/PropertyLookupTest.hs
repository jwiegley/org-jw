{-# LANGUAGE OverloadedStrings #-}

module PropertyLookupTest (tests) where

import Control.Lens (has, toListOf, (&), (.~), (^.), (^?))
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "f.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

props :: [Property]
props =
  [ Property loc0 False "ID" "abc-123"
  , Property loc0 False "CATEGORY" "work"
  ]

propsLower :: [Property]
propsLower = [Property loc0 False "id" "xyz"]

entryWith :: [Property] -> Entry
entryWith ps =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "foo"
    , _entryVerb = Nothing
    , _entryTitle = "foo"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = ps
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

sampleOrgFile :: OrgFile
sampleOrgFile =
  OrgFile
    { _orgFilePath = "sample.org"
    , _orgFileHeader =
        Header
          { _headerPropertiesDrawer = [Property loc0 False "ID" "file-1"]
          , _headerFileProperties = [Property loc0 False "TITLE" "Sample"]
          , _headerPreamble = emptyBody0
          }
    , _orgFileEntries = []
    }

tests :: TestTree
tests =
  testGroup
    "property lookup & inheritance"
    [ testGroup
        "lookupProperty gets"
        [ testCase "finds existing property value" $
            entryWith props ^? property "ID" @?= Just "abc-123"
        , testCase "returns Nothing for missing" $
            entryWith props ^? property "NOT_THERE" @?= Nothing
        , testCase "is case-insensitive on lookup key" $
            entryWith props ^? property "id" @?= Just "abc-123"
        , testCase "matches when prop name is lowercase" $
            entryWith propsLower ^? property "ID" @?= Just "xyz"
        , testCase "collects all values (traversal)" $
            toListOf (property "CATEGORY") (entryWith props) @?= ["work"]
        , testCase "empty property list yields empty traversal" $
            toListOf (property "ID") (entryWith []) @?= ([] :: [String])
        ]
    , testGroup
        "lookupProperty sets"
        [ testCase "updates value of existing property" $
            (entryWith props & property "ID" .~ "new")
              ^? property "ID"
              @?= Just "new"
        , testCase "updating missing property is no-op" $
            let e' = entryWith props & property "NOT_THERE" .~ "new"
             in e' ^. entryProperties @?= props
        , testCase "upper-case update via lowercase stored name" $
            (entryWith propsLower & property "ID" .~ "uuu")
              ^? property "ID"
              @?= Just "uuu"
        ]
    , testGroup
        "has + property"
        [ testCase "has detects presence" $
            has (property "ID") (entryWith props) @?= True
        , testCase "has returns False when absent" $
            has (property "NONE") (entryWith props) @?= False
        ]
    , testGroup
        "orgFileProperty"
        [ testCase "finds ID in PROPERTIES drawer" $
            sampleOrgFile ^? orgFileProperty "ID" @?= Just "file-1"
        , testCase "falls back to file-level properties" $
            sampleOrgFile ^? orgFileProperty "TITLE" @?= Just "Sample"
        , testCase "returns Nothing when missing in both" $
            sampleOrgFile ^? orgFileProperty "MISSING" @?= Nothing
        , testCase "drawer wins over file props when both present" $
            let o =
                  sampleOrgFile
                    { _orgFileHeader =
                        (_orgFileHeader sampleOrgFile)
                          { _headerPropertiesDrawer =
                              [Property loc0 False "TITLE" "drawer-title"]
                          , _headerFileProperties =
                              [Property loc0 False "TITLE" "file-title"]
                          }
                    }
             in o ^? orgFileProperty "TITLE" @?= Just "drawer-title"
        ]
    , testGroup
        "inheritProperties"
        [ testCase "adds inherited property when entry has none" $
            let inh = [Property loc0 False "CATEGORY" "parent"]
                e' = inheritProperties inh (entryWith [])
             in e' ^? property "CATEGORY" @?= Just "parent"
        , testCase "does not overwrite existing property" $
            let inh = [Property loc0 False "CATEGORY" "parent"]
                e' = inheritProperties inh (entryWith props)
             in e' ^? property "CATEGORY" @?= Just "work"
        , testCase "marks inherited properties as inherited=True" $
            let inh = [Property loc0 False "CATEGORY" "parent"]
                e' = inheritProperties inh (entryWith [])
                ps' = _entryProperties e'
             in case ps' of
                  [p] -> _inherited p @?= True
                  _ -> assertFailure ("expected one property, got " ++ show ps')
        , testCase "empty inherited list leaves entry alone" $
            inheritProperties [] (entryWith props) @?= entryWith props
        , testCase "multiple inherited properties" $
            let inh =
                  [ Property loc0 False "A" "1"
                  , Property loc0 False "B" "2"
                  ]
                e' = inheritProperties inh (entryWith [])
             in (e' ^? property "A", e' ^? property "B")
                  @?= (Just "1", Just "2")
        ]
    ]
