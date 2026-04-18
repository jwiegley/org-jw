{-# LANGUAGE OverloadedStrings #-}

module FoldEntriesTest (tests) where

import Control.Lens (toListOf, (^?))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as M
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "f.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkEntry :: String -> [Entry] -> Entry
mkEntry title children =
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
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = children
    }

mkEntryKw :: String -> Maybe Keyword -> [Entry] -> Entry
mkEntryKw title kw children = (mkEntry title children){_entryKeyword = kw}

mkEntryWithBody :: String -> Body -> [Entry] -> Entry
mkEntryWithBody title bdy children = (mkEntry title children){_entryBody = bdy}

mkFile :: FilePath -> Header -> [Entry] -> OrgFile
mkFile = OrgFile

-- Hierarchy: root has two items, one has a sub-item
nestedEntries :: [Entry]
nestedEntries =
  [ mkEntry "one" [mkEntry "one.a" [], mkEntry "one.b" []]
  , mkEntry "two" []
  ]

flatEntries :: [Entry]
flatEntries =
  [ mkEntryKw "a" (Just (OpenKeyword loc0 "TODO")) []
  , mkEntryKw "b" (Just (ClosedKeyword loc0 "DONE")) []
  , mkEntryKw "c" (Just (OpenKeyword loc0 "TODO")) []
  , mkEntryKw "d" Nothing []
  ]

sampleFile :: OrgFile
sampleFile = mkFile "s.org" (Header [] [] emptyBody0) nestedEntries

sampleFileFlat :: OrgFile
sampleFileFlat = mkFile "flat.org" (Header [] [] emptyBody0) flatEntries

sampleCollection :: Collection
sampleCollection =
  Collection
    [ OrgItem sampleFile
    , OrgItem sampleFileFlat
    , DataItem "unrelated.png"
    ]

-- Test inherited properties at the file-header level
fileWithInherited :: OrgFile
fileWithInherited =
  mkFile
    "inh.org"
    Header
      { _headerPropertiesDrawer = [Property loc0 False "CATEGORY" "work"]
      , _headerFileProperties = [Property loc0 False "ARCHIVE" "/dev/null"]
      , _headerPreamble = emptyBody0
      }
    [mkEntry "child" []]

-- Entry with inline task block in its body
inlineTaskChild :: Entry
inlineTaskChild = mkEntry "inline-child" []

fileWithInlineTask :: OrgFile
fileWithInlineTask =
  mkFile
    "inline.org"
    (Header [] [] emptyBody0)
    [ mkEntryWithBody
        "parent"
        (Body [InlineTask loc0 inlineTaskChild])
        []
    ]

tests :: TestTree
tests =
  testGroup
    "fold/traverse entries"
    [ testGroup
        "foldEntries"
        [ testCase "folds all nested entries into count" $
            foldEntries [] (\_ acc -> acc + (1 :: Int)) 0 nestedEntries @?= 4
        , testCase "empty list folds to seed" $
            foldEntries [] (\_ acc -> acc + (1 :: Int)) 0 [] @?= 0
        , testCase "fold visits all entries (order is pre-order)" $
            foldEntries [] (\e acc -> _entryHeadline e : acc) [] nestedEntries
              @?= ["one", "one.a", "one.b", "two"]
        ]
    , testGroup
        "traverseEntries"
        [ testCase "collects headlines via Identity applicative" $
            let names =
                  runIdentity $
                    traverseEntries [] (Identity . _entryHeadline) nestedEntries
             in names @?= ["one", "one.a", "one.b", "two"]
        , testCase "empty entries list yields empty collection" $
            let names =
                  runIdentity $
                    traverseEntries [] (Identity . _entryHeadline) []
             in names @?= ([] :: [String])
        ]
    , testGroup
        "entries traversal"
        [ testCase "visits all descendant entries" $
            length (toListOf (entries []) sampleFile) @?= 4
        , testCase "visits only top-level when no nesting" $
            length (toListOf (entries []) sampleFileFlat) @?= 4
        , testCase "headlines listed in pre-order" $
            map _entryHeadline (toListOf (entries []) sampleFile)
              @?= ["one", "one.a", "one.b", "two"]
        ]
    , testGroup
        "allEntries traversal"
        [ testCase "visits all descendant entries" $
            length (toListOf allEntries sampleFile) @?= 4
        , testCase "adds inherited CATEGORY to child entries" $
            fileWithInherited ^? allEntries . property "CATEGORY"
              @?= Just "work"
        , testCase "adds inherited ARCHIVE to child entries" $
            fileWithInherited ^? allEntries . property "ARCHIVE"
              @?= Just "/dev/null"
        , testCase "descends through inline tasks" $
            length (toListOf allEntries fileWithInlineTask) @?= 2
        ]
    , testGroup
        "foldAllEntries"
        [ testCase "counts all entries in a collection" $
            foldAllEntries sampleCollection (0 :: Int) (\_ n -> n + 1) @?= 8
        , testCase "empty collection yields seed" $
            foldAllEntries (Collection []) (42 :: Int) (\_ n -> n + 1) @?= 42
        , testCase "DataItems are ignored" $
            foldAllEntries
              (Collection [DataItem "x", DataItem "y"])
              (0 :: Int)
              (\_ n -> n + 1)
              @?= 0
        ]
    , testGroup
        "countEntries + tallyEntry"
        [ testCase "tally by keyword" $
            let byKw :: M.Map String Int
                byKw = countEntries sampleCollection $ \e m add ->
                  case _entryKeyword e of
                    Just (OpenKeyword _ k) -> add m k
                    Just (ClosedKeyword _ k) -> add m k
                    Nothing -> m
             in byKw @?= M.fromList [("DONE", 1), ("TODO", 2)]
        , testCase "tally by headline string" $
            let byName :: M.Map String Int
                byName = countEntries sampleCollection $ \e m add ->
                  add m (_entryHeadline e)
             in M.lookup "one" byName @?= Just 1
        , testCase "empty collection produces empty map" $
            let m :: M.Map String Int
                m = countEntries (Collection []) $ \e mp add ->
                  add mp (_entryHeadline e)
             in M.null m @?= True
        , testCase "tallyEntry increments an existing bucket" $
            let f :: Entry -> M.Map String Int -> (M.Map String Int -> String -> M.Map String Int) -> M.Map String Int
                f e mp add = add mp (_entryHeadline e)
                once = tallyEntry f (mkEntry "x" []) M.empty
                twice = tallyEntry f (mkEntry "x" []) once
             in M.lookup "x" twice @?= Just 2
        ]
    , testGroup
        "entriesMap via foldAllEntries"
        [ testCase "entriesMap builds map keyed by ID" $
            let e1 = (mkEntry "a" []){_entryProperties = [Property loc0 False "ID" "k"]}
                f =
                  OrgItem $
                    mkFile "x.org" (Header [] [] emptyBody0) [e1]
                m = entriesMap (Collection [f])
             in length <$> M.lookup "k" m @?= Just 1
        ]
    ]
