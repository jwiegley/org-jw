{-# LANGUAGE OverloadedStrings #-}

module MoreHelpersTest (tests) where

import Control.Lens (
  from,
  set,
  toListOf,
  view,
  (&),
  (.~),
  (^.),
  (^?),
 )
import qualified Data.Map.Strict as M
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "m.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

mkTime :: Integer -> Time
mkTime day =
  Time
    { _timeKind = InactiveTime
    , _timeDay = day
    , _timeDayEnd = Nothing
    , _timeStart = Nothing
    , _timeEnd = Nothing
    , _timeSuffix = Nothing
    }

baseEntry :: Entry
baseEntry =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "h"
    , _entryVerb = Nothing
    , _entryTitle = "t"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = emptyBody0
    , _entryItems = []
    }

mkOrgFile :: FilePath -> [Property] -> [Property] -> OrgFile
mkOrgFile p drawer fileProps =
  OrgFile
    { _orgFilePath = p
    , _orgFileHeader =
        Header
          { _headerPropertiesDrawer = drawer
          , _headerFileProperties = fileProps
          , _headerPreamble = emptyBody0
          }
    , _orgFileEntries = []
    }

tests :: TestTree
tests =
  testGroup
    "more helpers"
    [ testGroup
        "leadSpace / endSpace compound body"
        [ testCase "reads leading and trailing whitespace" $
            let b =
                  Body
                    [ Whitespace loc0 "  "
                    , Paragraph loc0 ["x"]
                    , Whitespace loc0 "\n"
                    ]
             in (b ^? leadSpace, b ^? endSpace)
                  @?= (Just "  ", Just "\n")
        , testCase "setting lead preserves rest" $
            let b =
                  Body
                    [ Whitespace loc0 " "
                    , Paragraph loc0 ["x"]
                    , Whitespace loc0 "\n"
                    ]
                b' = b & leadSpace .~ "!!"
             in b' ^? endSpace @?= Just "\n"
        , testCase "setting end preserves lead" $
            let b =
                  Body
                    [ Whitespace loc0 " "
                    , Paragraph loc0 ["x"]
                    , Whitespace loc0 "\n"
                    ]
                b' = b & endSpace .~ "##"
             in b' ^? leadSpace @?= Just " "
        ]
    , testGroup
        "entryCategory / entryId"
        [ testCase "entryId finds ID property" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "xyz"]}
             in e ^? entryId @?= Just "xyz"
        , testCase "entryId returns Nothing when absent" $
            baseEntry ^? entryId @?= Nothing
        , testCase "entryCategory finds CATEGORY property" $
            let e =
                  baseEntry
                    { _entryProperties =
                        [Property loc0 False "CATEGORY" "work"]
                    }
             in e ^? entryCategory @?= Just "work"
        , testCase "entryCategory returns Nothing when absent" $
            baseEntry ^? entryCategory @?= Nothing
        , testCase "entryId setter updates property" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "a"]}
             in (e & entryId .~ "b") ^? entryId @?= Just "b"
        ]
    , testGroup
        "entryTagString + tagList Iso roundtrip"
        [ testCase "from tagList parses common shape" $
            "::foo:bar::" ^. from tagList
              @?= [PlainTag "foo", PlainTag "bar"]
        , testCase "tagList forward is consistent" $
            [PlainTag "foo", PlainTag "bar"] ^. tagList
              @?= "::foo:bar::"
        , testCase "entryTagString reads serialized tags" $
            let e = baseEntry{_entryTags = [PlainTag "x", PlainTag "y"]}
             in e ^? entryTagString @?= Just "::x:y::"
        , testCase "entryTagString sets round-trips via iso" $
            let e =
                  baseEntry
                    & entryTagString .~ ":foo:bar:"
             in _entryTags e @?= [PlainTag "foo", PlainTag "bar"]
        ]
    , testGroup
        "createdTime / editedTime"
        [ testCase "createdTime via property" $
            let e =
                  baseEntry
                    { _entryProperties =
                        [Property loc0 False "CREATED" "[2024-06-01 Sat]"]
                    }
             in (e ^? createdTime) /= Nothing @?= True
        , testCase "createdTime absent when property missing" $
            baseEntry ^? createdTime @?= Nothing
        , testCase "editedTime via property" $
            let e =
                  baseEntry
                    { _entryProperties =
                        [Property loc0 False "EDITED" "[2024-06-01 Sat]"]
                    }
             in (e ^? editedTime) /= Nothing @?= True
        ]
    , testGroup
        "scheduled/deadline/closedTime stamp-based"
        [ testCase "scheduledTime via stamp" $
            let t = mkTime 60000
                e = baseEntry{_entryStamps = [ScheduledStamp loc0 t]}
             in e ^? scheduledTime @?= Just t
        , testCase "deadlineTime via stamp" $
            let t = mkTime 60000
                e = baseEntry{_entryStamps = [DeadlineStamp loc0 t]}
             in e ^? deadlineTime @?= Just t
        , testCase "closedTime via stamp" $
            let t = mkTime 60000
                e = baseEntry{_entryStamps = [ClosedStamp loc0 t]}
             in e ^? closedTime @?= Just t
        ]
    , testGroup
        "lookupProperty' case-insensitive"
        [ testCase "Property named 'Id' is found under lowercase 'id'" $
            let e = baseEntry{_entryProperties = [Property loc0 False "Id" "k"]}
             in e ^? property "id" @?= Just "k"
        , testCase "Property named 'Id' is found under uppercase 'ID'" $
            let e = baseEntry{_entryProperties = [Property loc0 False "Id" "k"]}
             in e ^? property "ID" @?= Just "k"
        , testCase "Property named 'ID' is found under mixed 'iD'" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "k"]}
             in e ^? property "iD" @?= Just "k"
        ]
    , testGroup
        "addRefToMap / addEntryToMap branches"
        [ testCase "addRefToMap creates empty bucket for fresh key" $
            M.lookup "k" (addRefToMap "k" M.empty)
              @?= Just ([] :: [Entry])
        , testCase "addRefToMap preserves existing bucket" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "k"]}
                m = addEntryToMap e M.empty
             in length <$> M.lookup "k" (addRefToMap "k" m) @?= Just 1
        , testCase "addEntryToMap fresh key yields singleton" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "k"]}
             in length <$> M.lookup "k" (addEntryToMap e M.empty)
                  @?= Just 1
        , testCase "addEntryToMap existing key prepends" $
            let a = baseEntry{_entryProperties = [Property loc0 False "ID" "k"]}
                b = baseEntry{_entryProperties = [Property loc0 False "ID" "k"]}
                m = addEntryToMap b (addEntryToMap a M.empty)
             in length <$> M.lookup "k" m @?= Just 2
        , testCase "addEntryToMap without ID uses empty key" $
            length <$> M.lookup "" (addEntryToMap baseEntry M.empty)
              @?= Just 1
        ]
    , testGroup
        "collectionPaths mix"
        [ testCase "paths for OrgItem and DataItem" $
            let c =
                  Collection
                    [ OrgItem (mkOrgFile "a.org" [] [])
                    , DataItem "b.png"
                    , OrgItem (mkOrgFile "c.org" [] [])
                    ]
             in collectionPaths c @?= ["a.org", "b.png", "c.org"]
        ]
    , testGroup
        "orgFileProperty drawer vs file-properties"
        [ testCase "drawer branch" $
            let o = mkOrgFile "x.org" [Property loc0 False "A" "1"] []
             in o ^? orgFileProperty "A" @?= Just "1"
        , testCase "file-properties branch" $
            let o = mkOrgFile "x.org" [] [Property loc0 False "B" "2"]
             in o ^? orgFileProperty "B" @?= Just "2"
        , testCase "missing from both yields Nothing" $
            let o = mkOrgFile "x.org" [] []
             in o ^? orgFileProperty "C" @?= Nothing
        ]
    , testGroup
        "entryStateHistory"
        [ testCase "biplate yields each top-level LogEntry" $
            let kw = OpenKeyword loc0 "TODO"
                st1 = LogState loc0 kw Nothing (mkTime 60000) Nothing
                st2 = LogState loc0 kw Nothing (mkTime 60001) Nothing
                e =
                  baseEntry
                    { _entryLogEntries = [st1, st2]
                    }
             in length (toListOf entryStateHistory e) @?= 2
        , testCase "top-level log entry is found" $
            let t = mkTime 60000
                e =
                  baseEntry
                    { _entryLogEntries = [LogNote loc0 t Nothing]
                    }
             in length (toListOf entryStateHistory e) @?= 1
        , testCase "empty logs yield no states" $
            toListOf entryStateHistory baseEntry @?= []
        ]
    , testGroup
        "fileSlug fold"
        [ testCase "OrgItem uses filename when no SLUG" $
            let f = mkOrgFile "20240601-my-note.org" [] []
             in OrgItem f ^? fileSlug @?= Just "my-note"
        , testCase "OrgItem with SLUG property uses it" $
            let f =
                  mkOrgFile
                    "20240601-ignored.org"
                    [Property loc0 False "SLUG" "custom-slug"]
                    []
             in OrgItem f ^? fileSlug @?= Just "custom-slug"
        , testCase "DataItem uses filename" $
            DataItem "20240601-img.png" ^? fileSlug @?= Just "img"
        ]
    , testGroup
        "fileActualSlug"
        [ testCase "OrgItem gets slug" $
            view fileActualSlug (OrgItem (mkOrgFile "20240601-foo.org" [] []))
              @?= "foo"
        , testCase "DataItem gets slug" $
            view fileActualSlug (DataItem "20240601-pic.png") @?= "pic"
        , testCase "setter changes slug only" $
            let item = OrgItem (mkOrgFile "20240601-foo.org" [] [])
                item' = set fileActualSlug "bar" item
             in view fileActualSlug item' @?= "bar"
        ]
    , testGroup
        "fileTitle"
        [ testCase "OrgItem without TITLE uses fileSlug" $
            OrgItem (mkOrgFile "20240601-my-note.org" [] [])
              ^? fileTitle
              @?= Just "my-note"
        , testCase "OrgItem with TITLE property uses it" $
            let f =
                  mkOrgFile
                    "20240601-ignored.org"
                    [Property loc0 False "TITLE" "My Real Title"]
                    []
             in OrgItem f ^? fileTitle @?= Just "My Real Title"
        , testCase "DataItem falls through fileActualSlug" $
            DataItem "20240601-x.png" ^? fileTitle @?= Just "x"
        ]
    , testGroup
        "filePath lens"
        [ testCase "OrgItem gets path" $
            view filePath (OrgItem (mkOrgFile "a.org" [] []))
              @?= "a.org"
        , testCase "DataItem gets path" $
            view filePath (DataItem "x.png") @?= "x.png"
        , testCase "OrgItem set path" $
            view
              filePath
              (set filePath "b.org" (OrgItem (mkOrgFile "a.org" [] [])))
              @?= "b.org"
        , testCase "DataItem set path" $
            view filePath (set filePath "y.png" (DataItem "x.png"))
              @?= "y.png"
        ]
    , testGroup
        "fileTags lens"
        [ testCase "OrgItem with filetags property reads them" $
            let f =
                  mkOrgFile
                    "misc.org"
                    []
                    [Property loc0 False "filetags" "::home:errands::"]
             in view fileTags (OrgItem f)
                  @?= [PlainTag "home", PlainTag "errands"]
        , testCase "OrgItem without filetags returns filename tags" $
            view fileTags (OrgItem (mkOrgFile "notes.org" [] []))
              @?= []
        , testCase "OrgItem with filename tag brackets reads them" $
            view
              fileTags
              ( OrgItem
                  (mkOrgFile "20240601-thing[work urgent].org" [] [])
              )
              @?= [PlainTag "work", PlainTag "urgent"]
        , testCase "OrgItem set tags (no filetags) updates filename tags" $
            let item =
                  OrgItem (mkOrgFile "20240601-thing[old].org" [] [])
                item' = set fileTags [PlainTag "foo"] item
             in view fileTags item' @?= [PlainTag "foo"]
        , testCase "OrgItem set tags (with filetags) writes back to property" $
            let f =
                  mkOrgFile
                    "m.org"
                    []
                    [Property loc0 False "filetags" "::old::"]
                item' = set fileTags [PlainTag "new"] (OrgItem f)
             in view fileTags item' @?= [PlainTag "new"]
        , testCase "DataItem reads filename tags" $
            view fileTags (DataItem "20240601-x[a b].png")
              @?= [PlainTag "a", PlainTag "b"]
        , testCase "DataItem sets filename tags" $
            view
              fileTags
              ( set
                  fileTags
                  [PlainTag "z"]
                  (DataItem "20240601-x[old].png")
              )
              @?= [PlainTag "z"]
        ]
    ]
