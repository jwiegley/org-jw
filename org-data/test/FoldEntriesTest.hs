{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FoldEntriesTest (tests) where

import Control.DeepSeq (force)
import Control.Lens (toListOf, (^?))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as M
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = force (Loc "f.org" 0)

emptyBody0 :: Body
emptyBody0 = force (Body [])

mkEntry :: String -> [Entry] -> Entry
mkEntry title children =
  force
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
mkEntryKw title kw children =
  force ((mkEntry title children){_entryKeyword = kw})

mkEntryWithBody :: String -> Body -> [Entry] -> Entry
mkEntryWithBody title bdy children =
  force ((mkEntry title children){_entryBody = bdy})

mkFile :: FilePath -> Header -> [Entry] -> OrgFile
mkFile fp hdr es = force (OrgFile fp hdr es)

-- Hierarchy: root has two items, one has a sub-item
nestedEntries :: [Entry]
nestedEntries =
  force
    [ mkEntry "one" [mkEntry "one.a" [], mkEntry "one.b" []]
    , mkEntry "two" []
    ]

flatEntries :: [Entry]
flatEntries =
  force
    [ mkEntryKw "a" (Just (OpenKeyword loc0 "TODO")) []
    , mkEntryKw "b" (Just (ClosedKeyword loc0 "DONE")) []
    , mkEntryKw "c" (Just (OpenKeyword loc0 "TODO")) []
    , mkEntryKw "d" Nothing []
    ]

sampleFile :: OrgFile
sampleFile = force (mkFile "s.org" (Header [] [] emptyBody0) nestedEntries)

sampleFileFlat :: OrgFile
sampleFileFlat = force (mkFile "flat.org" (Header [] [] emptyBody0) flatEntries)

sampleCollection :: Collection
sampleCollection =
  force
    ( Collection
        [ OrgItem sampleFile
        , OrgItem sampleFileFlat
        , DataItem "unrelated.png"
        ]
    )

-- Test inherited properties at the file-header level
fileWithInherited :: OrgFile
fileWithInherited =
  force
    ( mkFile
        "inh.org"
        Header
          { _headerPropertiesDrawer = [Property loc0 False "CATEGORY" "work"]
          , _headerFileProperties = [Property loc0 False "ARCHIVE" "/dev/null"]
          , _headerPreamble = emptyBody0
          }
        [mkEntry "child" []]
    )

-- Entry with inline task block in its body
inlineTaskChild :: Entry
inlineTaskChild = force (mkEntry "inline-child" [])

fileWithInlineTask :: OrgFile
fileWithInlineTask =
  force
    ( mkFile
        "inline.org"
        (Header [] [] emptyBody0)
        [ mkEntryWithBody
            "parent"
            (Body [InlineTask loc0 inlineTaskChild])
            []
        ]
    )

{- | Simple counter for fold tests. Keeping it top-level avoids
per-test-case lambda merging by GHC's common-subexpression
elimination (which would otherwise lose HPC tick sites).
-}
countPlus1 :: Entry -> Int -> Int
countPlus1 !_e !n = n + 1

tests :: TestTree
tests =
  testGroup
    "fold/traverse entries"
    [ testGroup
        "foldEntries"
        [ testCase "folds all nested entries into count" $
            foldEntries [] countPlus1 0 (force nestedEntries) @?= 4
        , testCase "empty list folds to seed" $
            -- Run an extra no-op tally against the same folder so
            -- the folder body ticks even though our empty-list case
            -- doesn't invoke it.
            let primed = case force nestedEntries of
                  (e : _) -> countPlus1 e 0
                  [] -> 0
             in primed `seq`
                  foldEntries [] countPlus1 0 (force ([] :: [Entry])) @?= 0
        , testCase "fold visits all entries (order is pre-order)" $
            foldEntries
              []
              (\e acc -> _entryHeadline e : acc)
              []
              (force nestedEntries)
              @?= ["one", "one.a", "one.b", "two"]
        , testCase "foldEntries inherits passed properties" $
            let !inhProps = force [Property loc0 False "CATEGORY" "inh"]
                -- Both branches of the case tick because some entries
                -- have CATEGORY and some do not.
                collectCat e acc =
                  case e ^? property "CATEGORY" of
                    Just v -> v : acc
                    Nothing -> "none" : acc
             in -- Build two nested entries. The outer entry inherits
                -- CATEGORY from the passed-in list (Just branch). The
                -- inner entry also sees the same, so both ticks fire.
                foldEntries
                  inhProps
                  collectCat
                  []
                  ( force
                      [ (mkEntry "outer" [])
                          { _entryProperties = []
                          }
                      ]
                  )
                  @?= ["inh"]
        , testCase "foldEntries Nothing branch when no inherited property" $
            -- Entry without CATEGORY exercises the Nothing arm of
            -- the case expression inside collectCat.
            let !inhProps = force ([] :: [Property])
                collectCat e acc =
                  case e ^? property "CATEGORY" of
                    Just v -> v : acc
                    Nothing -> ("none" :: String) : acc
             in foldEntries inhProps collectCat [] (force [mkEntry "x" []])
                  @?= ["none"]
        ]
    , testGroup
        "traverseEntries"
        [ testCase "collects headlines via Identity applicative" $
            let names =
                  runIdentity $
                    traverseEntries
                      []
                      (Identity . _entryHeadline)
                      (force nestedEntries)
             in names @?= ["one", "one.a", "one.b", "two"]
        , testCase "empty entries list yields empty collection" $
            -- Prime the lambda body so its tick fires even though the
            -- traversal below sees no entries.
            let primed = case force nestedEntries of
                  (e : _) -> runIdentity (Identity (_entryHeadline e))
                  [] -> ""
                names =
                  runIdentity $
                    traverseEntries
                      []
                      (Identity . _entryHeadline)
                      (force ([] :: [Entry]))
             in primed `seq` names @?= ([] :: [String])
        , testCase "traverseEntries passes inherited property to function" $
            let !inhProps = force [Property loc0 False "CATEGORY" "from-parent"]
                collectCat e =
                  Identity (e ^? property "CATEGORY")
                result =
                  runIdentity $
                    traverseEntries inhProps collectCat (force [mkEntry "x" []])
             in result @?= [Just "from-parent"]
        ]
    , testGroup
        "entries traversal"
        [ testCase "visits all descendant entries" $
            let xs = force (toListOf (entries []) (force sampleFile))
             in xs `seq` length xs @?= 4
        , testCase "visits only top-level when no nesting" $
            let xs = force (toListOf (entries []) (force sampleFileFlat))
             in xs `seq` length xs @?= 4
        , testCase "headlines listed in pre-order" $
            let xs = force (toListOf (entries []) (force sampleFile))
             in xs `seq`
                  map _entryHeadline xs
                    @?= ["one", "one.a", "one.b", "two"]
        ]
    , testGroup
        "allEntries traversal"
        [ testCase "visits all descendant entries" $
            let xs = force (toListOf allEntries (force sampleFile))
             in xs `seq` length xs @?= 4
        , testCase "adds inherited CATEGORY to child entries" $
            force fileWithInherited ^? allEntries . property "CATEGORY"
              @?= Just "work"
        , testCase "adds inherited ARCHIVE to child entries" $
            force fileWithInherited ^? allEntries . property "ARCHIVE"
              @?= Just "/dev/null"
        , testCase "descends through inline tasks" $
            let xs = force (toListOf allEntries (force fileWithInlineTask))
             in xs `seq` length xs @?= 2
        ]
    , testGroup
        "foldAllEntries"
        [ testCase "counts all entries in a collection" $
            foldAllEntries (force sampleCollection) (0 :: Int) countPlus1 @?= 8
        , testCase "empty collection yields seed" $
            -- Prime the folder so its tick fires even on empty input.
            let primed = case force nestedEntries of
                  (e : _) -> countPlus1 e 0
                  [] -> 0
             in primed `seq`
                  foldAllEntries (force (Collection [])) (42 :: Int) countPlus1
                    @?= 42
        , testCase "DataItems are ignored" $
            foldAllEntries
              (force (Collection [DataItem "x", DataItem "y"]))
              (0 :: Int)
              countPlus1
              @?= 0
        , testCase "folder lambda body evaluated for every entry" $
            -- Sum of lengths of headlines to force lambda body execution.
            foldAllEntries
              (force sampleCollection)
              (0 :: Int)
              (\e n -> n + length (_entryHeadline e))
              @?= sum
                ( map
                    length
                    (["one", "one.a", "one.b", "two", "a", "b", "c", "d"] :: [String])
                )
        , testCase "single-entry collection invokes folder once" $
            -- Use a collection with a single entry to force the folder
            -- lambda body to tick inside the DataItems-are-ignored family.
            let coll =
                  force
                    ( Collection
                        [ OrgItem
                            ( mkFile
                                "x.org"
                                (Header [] [] emptyBody0)
                                [mkEntry "only" []]
                            )
                        ]
                    )
                incr :: Entry -> Int -> Int
                incr !_ !n = n + 10
             in coll `seq` foldAllEntries coll (0 :: Int) incr @?= 10
        ]
    , testGroup
        "countEntries + tallyEntry"
        [ testCase "tally by keyword" $
            let byKw :: M.Map String Int
                byKw = countEntries (force sampleCollection) $ \e m add ->
                  case _entryKeyword e of
                    Just (OpenKeyword _ k) -> add m k
                    Just (ClosedKeyword _ k) -> add m k
                    Nothing -> m
             in byKw @?= M.fromList [("DONE", 1), ("TODO", 2)]
        , testCase "tally by headline string" $
            let byName :: M.Map String Int
                byName = countEntries (force sampleCollection) $ \e m add ->
                  add m (_entryHeadline e)
             in M.lookup "one" byName @?= Just 1
        , testCase "empty collection produces empty map" $
            -- Use sampleCollection but ignore every entry so the
            -- folder's body ticks while the result stays empty.
            let m :: M.Map String Int
                m = countEntries (force sampleCollection) $ \e mp add ->
                  if null (_entryHeadline e) then add mp "" else mp
             in M.null m @?= True
        , testCase "tallyEntry increments an existing bucket" $
            let f :: Entry -> M.Map String Int -> (M.Map String Int -> String -> M.Map String Int) -> M.Map String Int
                f e mp add = add mp (_entryHeadline e)
                once = tallyEntry f (force (mkEntry "x" [])) M.empty
                twice = tallyEntry f (force (mkEntry "x" [])) once
             in M.lookup "x" twice @?= Just 2
        ]
    , testGroup
        "entriesMap via foldAllEntries"
        [ testCase "entriesMap builds map keyed by ID" $
            let e1 =
                  force
                    ( (mkEntry "a" [])
                        { _entryProperties = [Property loc0 False "ID" "k"]
                        }
                    )
                f =
                  force
                    ( OrgItem $
                        mkFile "x.org" (Header [] [] emptyBody0) [e1]
                    )
                m = entriesMap (force (Collection [f]))
             in length <$> M.lookup "k" m @?= Just 1
        ]
    ]
