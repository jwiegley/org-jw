{-# LANGUAGE OverloadedStrings #-}

module CollectionMapTest (tests) where

import qualified Data.Map.Strict as M
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "x.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkEntry :: String -> Maybe String -> Entry
mkEntry title mid =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = title
    , _entryVerb = Nothing
    , _entryTitle = title
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties =
        case mid of
          Nothing -> []
          Just i -> [Property loc0 False "ID" i]
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

mkFile :: FilePath -> [Entry] -> OrgFile
mkFile p = OrgFile p (Header [] [] emptyBody0)

sampleCollection :: Collection
sampleCollection =
  Collection
    [ OrgItem (mkFile "a.org" [mkEntry "one" (Just "id-1")])
    , OrgItem (mkFile "b.org" [mkEntry "two" (Just "id-2"), mkEntry "three" Nothing])
    , DataItem "assets/pic.png"
    ]

tests :: TestTree
tests =
  testGroup
    "collection helpers"
    [ testGroup
        "collectionPaths"
        [ testCase "returns all paths in insertion order" $
            collectionPaths sampleCollection
              @?= ["a.org", "b.org", "assets/pic.png"]
        , testCase "empty collection yields empty list" $
            collectionPaths (Collection []) @?= []
        , testCase "only DataItems" $
            collectionPaths (Collection [DataItem "x", DataItem "y"])
              @?= ["x", "y"]
        ]
    , testGroup
        "addEntryToMap"
        [ testCase "inserts entry with ID into fresh map" $
            let e = mkEntry "hi" (Just "k")
                m = addEntryToMap e M.empty
             in M.lookup "k" m @?= Just [e]
        , testCase "prepends entry with same ID" $
            let e1 = mkEntry "a" (Just "k")
                e2 = mkEntry "b" (Just "k")
                m = addEntryToMap e2 (addEntryToMap e1 M.empty)
             in M.lookup "k" m @?= Just [e2, e1]
        , testCase "uses empty key when entry lacks ID" $
            let e = mkEntry "x" Nothing
             in M.lookup "" (addEntryToMap e M.empty) @?= Just [e]
        ]
    , testGroup
        "addRefToMap"
        [ testCase "adds empty bucket when key absent" $
            M.lookup "k" (addRefToMap "k" M.empty) @?= Just ([] :: [Entry])
        , testCase "leaves existing bucket unchanged" $
            let e = mkEntry "a" (Just "k")
                m0 = addEntryToMap e M.empty
             in M.lookup "k" (addRefToMap "k" m0) @?= Just [e]
        ]
    , testGroup
        "entriesMap"
        [ testCase "collects entries keyed by ID" $
            let m = entriesMap sampleCollection
             in ( length <$> M.lookup "id-1" m
                , length <$> M.lookup "id-2" m
                )
                  @?= (Just 1, Just 1)
        , testCase "entries without ID go under empty key" $
            let m = entriesMap sampleCollection
             in length <$> M.lookup "" m @?= Just 1
        , testCase "empty collection yields empty map" $
            entriesMap (Collection []) @?= (M.empty :: M.Map String [Entry])
        ]
    ]
