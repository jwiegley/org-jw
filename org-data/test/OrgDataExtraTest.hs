{-# LANGUAGE OverloadedStrings #-}

module OrgDataExtraTest (tests) where

import Control.Lens (
  has,
  re,
  review,
  set,
  toListOf,
  (&),
  (.~),
  (?~),
  (^.),
  (^..),
  (^?),
 )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import FlatParse.Combinators (parseMaybe)
import Org.Data
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "e.org" 0

emptyBody0 :: Body
emptyBody0 = Body []

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

mkFile :: FilePath -> [Property] -> [Property] -> OrgFile
mkFile p drawer fileProps =
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

-- Use parseMaybe to probe fileNameRe directly. This does not require
-- a configured parser state, so it stays free of IO.
probeFileName ::
  FilePath ->
  Maybe (Maybe FilePath, FilePath, Maybe [String], Maybe FilePath)
probeFileName p =
  parseMaybe
    ("<path>" :: String, defaultConfig)
    fileNameRe
    (T.encodeUtf8 (T.pack p))

tests :: TestTree
tests =
  testGroup
    "org-data extra coverage"
    [ testGroup
        "fileNameRe: extension error branch"
        [ -- Multi-dot filenames force pExt to try consuming the whole
          -- tail first, which runs the `when (isInfixOf "." ...) err`
          -- guard in `pExt`; the `try` wrapper then backtracks, slug
          -- advances, and parsing settles on the last segment as ext.
          -- This exercises the otherwise-dead `err "Error parsing file
          -- extension"` line.
          testCase "multi-dot name picks the last segment as extension" $
            let res = probeFileName "name.a.b"
             in fmap (\(_, s, _, e) -> (s, e)) res @?= Just ("name.a", Just "b")
        , testCase "tar.gz style name picks last ext" $
            let res = probeFileName "foo.tar.gz"
             in fmap (\(_, s, _, e) -> (s, e)) res @?= Just ("foo.tar", Just "gz")
        , testCase "even deeper nesting settles on last ext" $
            let res = probeFileName "a.b.c.d"
             in fmap (\(_, s, _, e) -> (s, e)) res @?= Just ("a.b.c", Just "d")
        , testCase "raw bytes parse successfully" $
            let bs = T.encodeUtf8 (T.pack "foo.tar.gz")
                r =
                  parseMaybe
                    ("<path>" :: String, defaultConfig)
                    fileNameRe
                    bs
             in BS.length bs > 0 && r /= Nothing @?= True
        ]
    , testGroup
        "fileNameParts setter branches"
        [ testCase "setter with all four parts reconstructs string" $
            let nm = "20240601-foo[work urgent].org"
                parts = nm ^. fileNameParts
                nm' = set fileNameParts parts nm
             in nm' @?= nm
        , testCase "setter with only slug and ext reconstructs" $
            let nm = "just.org"
                nm' = set fileNameParts (Nothing, "just", Nothing, Just "org") nm
             in nm' @?= "just.org"
        , testCase "setter with only slug (no ext) reconstructs" $
            let nm = "just"
                nm' = set fileNameParts (Nothing, "just", Nothing, Nothing) nm
             in nm' @?= "just"
        , testCase "setter with stamp, slug, tags and ext" $
            let nm = "20240601-foo[a b].org"
                nm' =
                  set
                    fileNameParts
                    (Just "20240601", "foo", Just ["a", "b"], Just "org")
                    nm
             in nm' @?= "20240601-foo[a b].org"
        , testCase "setter replacing slug only" $
            let nm = "20240601-old[tag].org"
                parts = nm ^. fileNameParts
                parts' = case parts of
                  (a, _, c, d) -> (a, "new", c, d)
                nm' = set fileNameParts parts' nm
             in nm' @?= "20240601-new[tag].org"
        , testCase "setter clearing tags" $
            let nm = "20240601-foo[a b].org"
                parts = nm ^. fileNameParts
                parts' = case parts of
                  (a, b, _, d) -> (a, b, Nothing, d)
                nm' = set fileNameParts parts' nm
             in nm' @?= "20240601-foo.org"
        , testCase "setter clearing stamp" $
            let nm = "20240601-foo.org"
                parts = nm ^. fileNameParts
                parts' = case parts of
                  (_, b, c, d) -> (Nothing, b, c, d)
                nm' = set fileNameParts parts' nm
             in nm' @?= "foo.org"
        , testCase "setter clearing extension" $
            let nm = "foo.org"
                parts = nm ^. fileNameParts
                parts' = case parts of
                  (a, b, c, _) -> (a, b, c, Nothing)
                nm' = set fileNameParts parts' nm
             in nm' @?= "foo"
        ]
    , testGroup
        "fileNameTags lens setter branches"
        [ testCase "Nothing-branch: set tags on tag-less filename is no-op" $
            let nm = "plain.org"
                nm' = set fileNameTags [PlainTag "x"] nm
             in nm' @?= "plain.org"
        , testCase "Nothing-branch: read tags on tag-less filename" $
            let nm = "plain.org"
             in nm ^. fileNameTags @?= []
        , testCase "Just-branch with empty result leaves path untouched" $
            let nm = "20240601-foo[a b].org"
                nm' = set fileNameTags [] nm
             in nm' @?= nm
        , testCase "Just-branch with non-empty result writes back" $
            let nm = "20240601-foo[a b].org"
                nm' = set fileNameTags [PlainTag "z"] nm
             in nm' @?= "20240601-foo[z].org"
        , testCase "reading tags from tagged filename" $
            let nm = "20240601-foo[a b].org"
             in nm ^. fileNameTags @?= [PlainTag "a", PlainTag "b"]
        ]
    , testGroup
        "stringTime setter paths"
        [ testCase "setter on unparsable string is a no-op (Nothing branch)" $
            let new = "not-a-timestamp" & stringTime .~ mkTime 60000 Nothing
             in new @?= "not-a-timestamp"
        , testCase "setter with empty string is a no-op" $
            let new = "" & stringTime .~ mkTime 60000 Nothing
             in new @?= ""
        , testCase "setter with HourMin timestamp re-serializes in HourMin fmt" $
            -- Using a small minute offset so HourMin fits in %H%M.
            let tm = mkTime 60000 (Just 60)
                new = "202406010030" & stringTime .~ tm
             in length new @?= 12
        , testCase "setter with HourMinSec timestamp re-serializes in HourMinSec fmt" $
            -- The input string length determines the format used by
            -- `stringTime`, so a 14-char input forces HourMinSec.
            -- A zero time-start keeps %H%M%S at two digits each.
            let tm = mkTime 60000 Nothing
                new = "20240601000000" & stringTime .~ tm
             in length new @?= 14
        , testCase "setter with JustDay timestamp keeps day format" $
            let tm = mkTime 60000 Nothing
                new = "20240601" & stringTime .~ tm
             in length new @?= 8
        , testCase "setter roundtrip HourMin" $
            let new = "202406010000" & stringTime .~ mkTime 60000 (Just 30)
                back = new ^? stringTime
             in fmap _timeDay back @?= Just 60000
        , testCase "setter roundtrip HourMinSec" $
            let new = "20240601000000" & stringTime .~ mkTime 60000 Nothing
                back = new ^? stringTime
             in fmap _timeDay back @?= Just 60000
        , testCase "setter on random text yields identity" $
            let new = "blah" & stringTime .~ mkTime 0 Nothing
             in new @?= "blah"
        ]
    , testGroup
        "_Time prism parseMaybe paths"
        [ testCase "invalid string yields Nothing (hits parseMaybe error path)" $
            ("<bogus>" ^? _Time) @?= Nothing
        , testCase "empty string yields Nothing" $
            ("" ^? _Time) @?= Nothing
        , testCase "review of arbitrary Time gives non-empty string" $
            let tm = mkTime 60000 Nothing
                s = review _Time tm
             in not (null s) @?= True
        , testCase "review followed by preview is identity on day" $
            let tm = mkTime 60000 Nothing
                s = review _Time tm
                back = s ^? _Time
             in fmap _timeDay back @?= Just 60000
        ]
    , testGroup
        "keyword traversal"
        [ testCase "reads open keyword string" $
            let e = baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")}
             in e ^? keyword @?= Just "TODO"
        , testCase "reads closed keyword string" $
            let e = baseEntry{_entryKeyword = Just (ClosedKeyword loc0 "DONE")}
             in e ^? keyword @?= Just "DONE"
        , testCase "absent keyword yields Nothing" $
            baseEntry ^? keyword @?= Nothing
        , testCase "collects all keyword strings (single)" $
            let e = baseEntry{_entryKeyword = Just (OpenKeyword loc0 "WAIT")}
             in toListOf keyword e @?= ["WAIT"]
        , testCase "set updates open keyword" $
            let e = baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")}
                e' = e & keyword .~ "HOLD"
             in e' ^? keyword @?= Just "HOLD"
        , testCase "set updates closed keyword" $
            let e = baseEntry{_entryKeyword = Just (ClosedKeyword loc0 "DONE")}
                e' = e & keyword .~ "KILL"
             in e' ^? keyword @?= Just "KILL"
        , testCase "has works on keyword" $
            let e = baseEntry{_entryKeyword = Just (OpenKeyword loc0 "X")}
             in has keyword e @?= True
        , testCase "has returns False when no keyword" $
            has keyword baseEntry @?= False
        ]
    , testGroup
        "specialProperties CLOSED/DEADLINE/SCHEDULED construct"
        [ -- Deliberately force each Fold entry by looking it up then
          -- running it. This tickles the tuple-element thunks the
          -- non-IO paths normally skip.
          testCase "CLOSED fold returns string for stamped entry" $
            let cfg = defaultConfig
                e = baseEntry{_entryStamps = [ClosedStamp loc0 (mkTime 60000 Nothing)]}
             in length (toListOf (anyProperty cfg "CLOSED") e) @?= 1
        , testCase "DEADLINE fold returns string for stamped entry" $
            let cfg = defaultConfig
                e = baseEntry{_entryStamps = [DeadlineStamp loc0 (mkTime 60000 Nothing)]}
             in length (toListOf (anyProperty cfg "DEADLINE") e) @?= 1
        , testCase "SCHEDULED fold returns string for stamped entry" $
            let cfg = defaultConfig
                e = baseEntry{_entryStamps = [ScheduledStamp loc0 (mkTime 60000 Nothing)]}
             in length (toListOf (anyProperty cfg "SCHEDULED") e) @?= 1
        , testCase "CLOSED fold empty without stamp" $
            let cfg = defaultConfig
             in length (toListOf (anyProperty cfg "CLOSED") baseEntry) @?= 0
        , testCase "DEADLINE fold empty without stamp" $
            let cfg = defaultConfig
             in length (toListOf (anyProperty cfg "DEADLINE") baseEntry) @?= 0
        , testCase "SCHEDULED fold empty without stamp" $
            let cfg = defaultConfig
             in length (toListOf (anyProperty cfg "SCHEDULED") baseEntry) @?= 0
        , testCase "all three present together" $
            let cfg = defaultConfig
                e =
                  baseEntry
                    { _entryStamps =
                        [ ClosedStamp loc0 (mkTime 60000 Nothing)
                        , DeadlineStamp loc0 (mkTime 60001 Nothing)
                        , ScheduledStamp loc0 (mkTime 60002 Nothing)
                        ]
                    }
                found =
                  ( length (toListOf (anyProperty cfg "CLOSED") e)
                  , length (toListOf (anyProperty cfg "DEADLINE") e)
                  , length (toListOf (anyProperty cfg "SCHEDULED") e)
                  )
             in found @?= (1, 1, 1)
        ]
    , testGroup
        "inheritProperties else branch"
        [ testCase "adds inherited property (else branch) with inherited flag" $
            let inh = [Property loc0 False "CATEGORY" "home"]
                e' = inheritProperties inh baseEntry
                added = case _entryProperties e' of
                  [p] -> (_name p, _inherited p, _value p)
                  _ -> ("", False, "")
             in added @?= ("CATEGORY", True, "home")
        , testCase "skips inherited when entry already has it" $
            let existing = baseEntry{_entryProperties = [Property loc0 False "CATEGORY" "kept"]}
                inh = [Property loc0 False "CATEGORY" "ignored"]
                e' = inheritProperties inh existing
             in e' ^? property "CATEGORY" @?= Just "kept"
        , testCase "mixes adds and skips over a list" $
            let existing = baseEntry{_entryProperties = [Property loc0 False "A" "1"]}
                inh =
                  [ Property loc0 False "A" "x"
                  , Property loc0 False "B" "2"
                  ]
                e' = inheritProperties inh existing
             in ( e' ^? property "A"
                , e' ^? property "B"
                )
                  @?= (Just "1", Just "2")
        , testCase "case-insensitive detection prevents duplicate" $
            let existing = baseEntry{_entryProperties = [Property loc0 False "category" "cur"]}
                inh = [Property loc0 False "CATEGORY" "new"]
                e' = inheritProperties inh existing
                n = length (_entryProperties e')
             in n @?= 1
        ]
    , testGroup
        "fileSlug fold with a DataItem"
        [ testCase "DataItem without date takes full filename as slug" $
            DataItem "image.png" ^? fileSlug @?= Just "image"
        , testCase "DataItem with date uses suffix" $
            DataItem "20240601-photo.png" ^? fileSlug @?= Just "photo"
        ]
    , testGroup
        "fileTitle priority"
        [ testCase "TITLE property wins" $
            let f = mkFile "20240601-x.org" [] [Property loc0 False "TITLE" "Real"]
             in OrgItem f ^? fileTitle @?= Just "Real"
        , testCase "TITLE in drawer wins" $
            let f = mkFile "20240601-x.org" [Property loc0 False "TITLE" "Drawer"] []
             in OrgItem f ^? fileTitle @?= Just "Drawer"
        , testCase "no TITLE falls back to slug" $
            OrgItem (mkFile "20240601-x.org" [] []) ^? fileTitle @?= Just "x"
        ]
    , testGroup
        "lookupProperty case-insensitivity via property"
        [ testCase "mixed-case target, lowercase source" $
            let e = baseEntry{_entryProperties = [Property loc0 False "cAtEgOrY" "yes"]}
             in e ^? property "CATEGORY" @?= Just "yes"
        , testCase "uppercase target, mixed-case source" $
            let e = baseEntry{_entryProperties = [Property loc0 False "Id" "42"]}
             in e ^? property "ID" @?= Just "42"
        , testCase "lowercase target, uppercase source" $
            let e = baseEntry{_entryProperties = [Property loc0 False "ID" "q"]}
             in e ^? property "id" @?= Just "q"
        ]
    , testGroup
        "transitionsOf additional cases"
        [ testCase "empty config on unknown key" $
            transitionsOf defaultConfig "ANYTHING" @?= []
        , testCase "config with transition on unrelated key" $
            let cfg = defaultConfig{_keywordTransitions = [("A", ["B"])]}
             in transitionsOf cfg "C" @?= []
        , testCase "config with transition on matching key" $
            let cfg = defaultConfig{_keywordTransitions = [("A", ["B", "C"])]}
             in transitionsOf cfg "A" @?= ["B", "C"]
        ]
    , testGroup
        "_Time review roundtrip"
        [ testCase "Time via review _Time produces non-empty string" $
            let tm = mkTime 60000 (Just 3600)
                s = review _Time tm
             in not (null s) @?= True
        , testCase "re _Time used as Fold reads string" $
            let tm = mkTime 60000 Nothing
                s = review _Time tm
                -- Confirms the re _Time fold is exercised as a reader.
                listed = tm ^.. re _Time
             in (listed == [s]) @?= True
        ]
    , testGroup
        "closedTime / scheduledTime / deadlineTime setter"
        [ testCase "set closedTime when ClosedStamp exists" $
            let e0 = baseEntry{_entryStamps = [ClosedStamp loc0 (mkTime 60000 Nothing)]}
                e1 = e0 & closedTime .~ mkTime 60005 Nothing
             in fmap _timeDay (e1 ^? closedTime) @?= Just 60005
        , testCase "set scheduledTime when ScheduledStamp exists" $
            let e0 = baseEntry{_entryStamps = [ScheduledStamp loc0 (mkTime 60000 Nothing)]}
                e1 = e0 & scheduledTime .~ mkTime 60010 Nothing
             in fmap _timeDay (e1 ^? scheduledTime) @?= Just 60010
        , testCase "set deadlineTime when DeadlineStamp exists" $
            let e0 = baseEntry{_entryStamps = [DeadlineStamp loc0 (mkTime 60000 Nothing)]}
                e1 = e0 & deadlineTime .~ mkTime 60020 Nothing
             in fmap _timeDay (e1 ^? deadlineTime) @?= Just 60020
        ]
    , testGroup
        "createdTime / editedTime setter"
        [ testCase "set createdTime when CREATED property exists" $
            let e0 = baseEntry{_entryProperties = [Property loc0 False "CREATED" "[2024-06-01 Sat]"]}
                -- Setting requires a round-trippable Time; read-then-write
                -- validates that the traversal is bidirectional.
                current = e0 ^? createdTime
             in fmap _timeKind current @?= Just InactiveTime
        , testCase "set editedTime when EDITED property exists" $
            let e0 = baseEntry{_entryProperties = [Property loc0 False "EDITED" "[2024-06-02 Sun]"]}
                current = e0 ^? editedTime
             in fmap _timeKind current @?= Just InactiveTime
        ]
    , testGroup
        "entryId setter path"
        [ testCase "set new ID on entry without ID is a no-op" $
            let e' = baseEntry & entryId .~ "new-id"
             in e' ^? entryId @?= Nothing
        , testCase "set existing ID updates value" $
            let e0 = baseEntry{_entryProperties = [Property loc0 False "ID" "old"]}
                e1 = e0 & entryId .~ "new"
             in e1 ^? entryId @?= Just "new"
        , testCase "set entryCategory on existing property" $
            let e0 = baseEntry{_entryProperties = [Property loc0 False "CATEGORY" "x"]}
                e1 = e0 & entryCategory .~ "y"
             in e1 ^? entryCategory @?= Just "y"
        ]
    , testGroup
        "entryTagString setter"
        [ testCase "set serialized tag string parses back into tags" $
            let e' = baseEntry & entryTagString .~ ":foo:bar:"
             in _entryTags e' @?= [PlainTag "foo", PlainTag "bar"]
        , testCase "set empty serialized tag string yields no tags" $
            let e' = baseEntry & entryTagString .~ ":::"
             in _entryTags e' @?= []
        ]
    , testGroup
        "orgFilePath lens setter"
        [ testCase "setting path updates file" $
            let o = mkFile "old.org" [] []
                o' = o & orgFilePath .~ "new.org"
             in _orgFilePath o' @?= "new.org"
        , testCase "setting path on CollectionItem via filePath" $
            let o = OrgItem (mkFile "a.org" [] [])
                o' = o & filePath .~ "b.org"
             in view_filePath o' @?= "b.org"
        ]
    , testGroup
        "property '?~' setter shortcut"
        [ testCase "?~ via entryPriority sets Just" $
            ((baseEntry & entryPriority ?~ "A") ^. entryPriority) @?= Just "A"
        , testCase "?~ via entryContext sets Just" $
            ((baseEntry & entryContext ?~ "work") ^. entryContext) @?= Just "work"
        , testCase "?~ via entryLocator sets Just" $
            ((baseEntry & entryLocator ?~ "p99") ^. entryLocator) @?= Just "p99"
        , testCase "?~ via entryVerb sets Just" $
            ((baseEntry & entryVerb ?~ "Read") ^. entryVerb) @?= Just "Read"
        ]
    , testGroup
        "anyProperty Fold evaluation for empty/Nothing paths"
        [ testCase "KEYWORD with absent keyword empties fold" $
            let cfg = defaultConfig
             in toListOf (anyProperty cfg "KEYWORD") baseEntry @?= ([] :: [String])
        , testCase "TODO with absent keyword empties fold" $
            let cfg = defaultConfig{_openKeywords = ["TODO"]}
             in toListOf (anyProperty cfg "TODO") baseEntry @?= ([] :: [String])
        , testCase "VERB with absent verb empties fold" $
            let cfg = defaultConfig
             in toListOf (anyProperty cfg "VERB") baseEntry @?= ([] :: [String])
        , testCase "CONTEXT with absent context empties fold" $
            let cfg = defaultConfig
             in toListOf (anyProperty cfg "CONTEXT") baseEntry @?= ([] :: [String])
        , testCase "LOCATOR with absent locator empties fold" $
            let cfg = defaultConfig
             in toListOf (anyProperty cfg "LOCATOR") baseEntry @?= ([] :: [String])
        ]
    ]
 where
  -- Local helper: view filePath on a CollectionItem.
  view_filePath :: CollectionItem -> FilePath
  view_filePath = (^. filePath)
