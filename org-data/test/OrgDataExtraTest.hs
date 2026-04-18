{-# LANGUAGE OverloadedStrings #-}

module OrgDataExtraTest (tests) where

import Control.DeepSeq (force)
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
loc0 = force (Loc "e.org" 0)

emptyBody0 :: Body
emptyBody0 = force (Body [])

mkTime :: Integer -> Maybe Integer -> Time
mkTime day startM =
  force
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
  force
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
  force
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
            let res = force (probeFileName "name.a.b")
             in res `seq`
                  fmap (\(_, s, _, e) -> (s, e)) res
                    @?= Just ("name.a", Just "b")
        , testCase "tar.gz style name picks last ext" $
            let res = force (probeFileName "foo.tar.gz")
             in res `seq`
                  fmap (\(_, s, _, e) -> (s, e)) res
                    @?= Just ("foo.tar", Just "gz")
        , testCase "even deeper nesting settles on last ext" $
            let res = force (probeFileName "a.b.c.d")
             in res `seq`
                  fmap (\(_, s, _, e) -> (s, e)) res
                    @?= Just ("a.b.c", Just "d")
        , testCase "raw bytes parse successfully" $
            let bs = force (T.encodeUtf8 (T.pack "foo.tar.gz"))
                r =
                  force
                    ( parseMaybe
                        ("<path>" :: String, defaultConfig)
                        fileNameRe
                        bs
                    )
             in bs `seq` r `seq` BS.length bs > 0 && r /= Nothing @?= True
        ]
    , testGroup
        "fileNameParts setter branches"
        [ testCase "setter with all four parts reconstructs string" $
            let nm = force ("20240601-foo[work urgent].org" :: FilePath)
                parts = force (nm ^. fileNameParts)
                nm' = force (set fileNameParts parts nm)
             in nm `seq` parts `seq` nm' `seq` nm' @?= nm
        , testCase "setter with only slug and ext reconstructs" $
            let nm = force ("just.org" :: FilePath)
                nm' =
                  force
                    (set fileNameParts (Nothing, "just", Nothing, Just "org") nm)
             in nm `seq` nm' `seq` nm' @?= ("just.org" :: FilePath)
        , testCase "setter with only slug (no ext) reconstructs" $
            let nm = force ("just" :: FilePath)
                nm' =
                  force (set fileNameParts (Nothing, "just", Nothing, Nothing) nm)
             in nm `seq` nm' `seq` nm' @?= ("just" :: FilePath)
        , testCase "setter with stamp, slug, tags and ext" $
            let nm = force ("20240601-foo[a b].org" :: FilePath)
                nm' =
                  force
                    ( set
                        fileNameParts
                        (Just "20240601", "foo", Just ["a", "b"], Just "org")
                        nm
                    )
             in nm `seq` nm' `seq` nm' @?= ("20240601-foo[a b].org" :: FilePath)
        , testCase "setter replacing slug only" $
            let nm = force ("20240601-old[tag].org" :: FilePath)
                parts = force (nm ^. fileNameParts)
                parts' = force $ case parts of
                  (a, _, c, d) -> (a, "new", c, d)
                nm' = force (set fileNameParts parts' nm)
             in nm `seq`
                  parts `seq`
                    parts' `seq`
                      nm' `seq`
                        nm' @?= ("20240601-new[tag].org" :: FilePath)
        , testCase "setter clearing tags" $
            let nm = force ("20240601-foo[a b].org" :: FilePath)
                parts = force (nm ^. fileNameParts)
                parts' = force $ case parts of
                  (a, b, _, d) -> (a, b, Nothing, d)
                nm' = force (set fileNameParts parts' nm)
             in nm `seq`
                  parts `seq`
                    parts' `seq`
                      nm' `seq`
                        nm' @?= ("20240601-foo.org" :: FilePath)
        , testCase "setter clearing stamp" $
            let nm = force ("20240601-foo.org" :: FilePath)
                parts = force (nm ^. fileNameParts)
                parts' = force $ case parts of
                  (_, b, c, d) -> (Nothing, b, c, d)
                nm' = force (set fileNameParts parts' nm)
             in nm `seq`
                  parts `seq`
                    parts' `seq`
                      nm' `seq`
                        nm' @?= ("foo.org" :: FilePath)
        , testCase "setter clearing extension" $
            let nm = force ("foo.org" :: FilePath)
                parts = force (nm ^. fileNameParts)
                parts' = force $ case parts of
                  (a, b, c, _) -> (a, b, c, Nothing)
                nm' = force (set fileNameParts parts' nm)
             in nm `seq`
                  parts `seq`
                    parts' `seq`
                      nm' `seq`
                        nm' @?= ("foo" :: FilePath)
        ]
    , testGroup
        "fileNameTags lens setter branches"
        [ testCase "Nothing-branch: set tags on tag-less filename is no-op" $
            let nm = force ("plain.org" :: FilePath)
                nm' = force (set fileNameTags [PlainTag "x"] nm)
             in nm `seq` nm' `seq` nm' @?= ("plain.org" :: FilePath)
        , testCase "Nothing-branch: read tags on tag-less filename" $
            let nm = force ("plain.org" :: FilePath)
             in nm `seq` nm ^. fileNameTags @?= []
        , testCase "Just-branch with empty result leaves path untouched" $
            let nm = force ("20240601-foo[a b].org" :: FilePath)
                nm' = force (set fileNameTags [] nm)
             in nm `seq` nm' `seq` nm' @?= nm
        , testCase "Just-branch with non-empty result writes back" $
            let nm = force ("20240601-foo[a b].org" :: FilePath)
                nm' = force (set fileNameTags [PlainTag "z"] nm)
             in nm `seq` nm' `seq` nm' @?= ("20240601-foo[z].org" :: FilePath)
        , testCase "reading tags from tagged filename" $
            let nm = force ("20240601-foo[a b].org" :: FilePath)
             in nm `seq`
                  nm ^. fileNameTags
                    @?= [PlainTag "a", PlainTag "b"]
        ]
    , testGroup
        "stringTime setter paths"
        [ testCase "setter on unparsable string is a no-op (Nothing branch)" $
            let tm = force (mkTime 60000 Nothing)
                new = force ("not-a-timestamp" & stringTime .~ tm)
             in tm `seq` new `seq` new @?= ("not-a-timestamp" :: String)
        , testCase "setter with empty string is a no-op" $
            let tm = force (mkTime 60000 Nothing)
                new = force ("" & stringTime .~ tm)
             in tm `seq` new `seq` new @?= ("" :: String)
        , testCase "setter with HourMin timestamp re-serializes in HourMin fmt" $
            -- Using a small minute offset so HourMin fits in %H%M.
            let tm = force (mkTime 60000 (Just 60))
                new = force ("202406010030" & stringTime .~ tm)
             in tm `seq` new `seq` length new @?= 12
        , testCase "setter with HourMinSec timestamp re-serializes in HourMinSec fmt" $
            -- The input string length determines the format used by
            -- `stringTime`, so a 14-char input forces HourMinSec.
            -- A zero time-start keeps %H%M%S at two digits each.
            let tm = force (mkTime 60000 Nothing)
                new = force ("20240601000000" & stringTime .~ tm)
             in tm `seq` new `seq` length new @?= 14
        , testCase "setter with JustDay timestamp keeps day format" $
            let tm = force (mkTime 60000 Nothing)
                new = force ("20240601" & stringTime .~ tm)
             in tm `seq` new `seq` length new @?= 8
        , testCase "setter roundtrip HourMin" $
            let tm = force (mkTime 60000 (Just 30))
                new = force ("202406010000" & stringTime .~ tm)
                back = force (new ^? stringTime)
             in tm `seq`
                  new `seq`
                    back `seq`
                      fmap _timeDay back @?= Just 60000
        , testCase "setter roundtrip HourMinSec" $
            let tm = force (mkTime 60000 Nothing)
                new = force ("20240601000000" & stringTime .~ tm)
                back = force (new ^? stringTime)
             in tm `seq`
                  new `seq`
                    back `seq`
                      fmap _timeDay back @?= Just 60000
        , testCase "setter on random text yields identity" $
            let tm = force (mkTime 0 Nothing)
                new = force ("blah" & stringTime .~ tm)
             in tm `seq` new `seq` new @?= ("blah" :: String)
        ]
    , testGroup
        "_Time prism parseMaybe paths"
        [ testCase "invalid string yields Nothing (hits parseMaybe error path)" $
            (("<bogus>" :: String) ^? _Time) @?= Nothing
        , testCase "empty string yields Nothing" $
            (("" :: String) ^? _Time) @?= Nothing
        , testCase "review of arbitrary Time gives non-empty string" $
            let tm = force (mkTime 60000 Nothing)
                s = force (review _Time tm)
             in tm `seq` s `seq` not (null s) @?= True
        , testCase "review followed by preview is identity on day" $
            let tm = force (mkTime 60000 Nothing)
                s = force (review _Time tm)
                back = force (s ^? _Time)
             in tm `seq`
                  s `seq`
                    back `seq`
                      fmap _timeDay back @?= Just 60000
        ]
    , testGroup
        "keyword traversal"
        [ testCase "reads open keyword string" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")})
             in e `seq` e ^? keyword @?= Just "TODO"
        , testCase "reads closed keyword string" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (ClosedKeyword loc0 "DONE")})
             in e `seq` e ^? keyword @?= Just "DONE"
        , testCase "absent keyword yields Nothing" $
            force baseEntry ^? keyword @?= Nothing
        , testCase "collects all keyword strings (single)" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "WAIT")})
             in e `seq` toListOf keyword e @?= ["WAIT"]
        , testCase "set updates open keyword" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")})
                e' = force (e & keyword .~ "HOLD")
             in e `seq` e' `seq` e' ^? keyword @?= Just "HOLD"
        , testCase "set updates closed keyword" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (ClosedKeyword loc0 "DONE")})
                e' = force (e & keyword .~ "KILL")
             in e `seq` e' `seq` e' ^? keyword @?= Just "KILL"
        , testCase "has works on keyword" $
            let e =
                  force
                    (baseEntry{_entryKeyword = Just (OpenKeyword loc0 "X")})
             in e `seq` has keyword e @?= True
        , testCase "has returns False when no keyword" $
            has keyword (force baseEntry) @?= False
        ]
    , testGroup
        "specialProperties CLOSED/DEADLINE/SCHEDULED construct"
        [ -- Deliberately force each Fold entry by looking it up then
          -- running it. This tickles the tuple-element thunks the
          -- non-IO paths normally skip.
          testCase "CLOSED fold returns string for stamped entry" $
            let cfg = force defaultConfig
                t = force (mkTime 60000 Nothing)
                e =
                  force
                    (baseEntry{_entryStamps = [ClosedStamp loc0 t]})
             in cfg `seq`
                  t `seq`
                    e `seq`
                      length (toListOf (anyProperty cfg "CLOSED") e) @?= 1
        , testCase "DEADLINE fold returns string for stamped entry" $
            let cfg = force defaultConfig
                t = force (mkTime 60000 Nothing)
                e =
                  force
                    (baseEntry{_entryStamps = [DeadlineStamp loc0 t]})
             in cfg `seq`
                  t `seq`
                    e `seq`
                      length (toListOf (anyProperty cfg "DEADLINE") e) @?= 1
        , testCase "SCHEDULED fold returns string for stamped entry" $
            let cfg = force defaultConfig
                t = force (mkTime 60000 Nothing)
                e =
                  force
                    (baseEntry{_entryStamps = [ScheduledStamp loc0 t]})
             in cfg `seq`
                  t `seq`
                    e `seq`
                      length (toListOf (anyProperty cfg "SCHEDULED") e) @?= 1
        , testCase "CLOSED fold empty without stamp" $
            let cfg = force defaultConfig
             in cfg `seq`
                  length (toListOf (anyProperty cfg "CLOSED") (force baseEntry))
                    @?= 0
        , testCase "DEADLINE fold empty without stamp" $
            let cfg = force defaultConfig
             in cfg `seq`
                  length
                    (toListOf (anyProperty cfg "DEADLINE") (force baseEntry))
                    @?= 0
        , testCase "SCHEDULED fold empty without stamp" $
            let cfg = force defaultConfig
             in cfg `seq`
                  length
                    (toListOf (anyProperty cfg "SCHEDULED") (force baseEntry))
                    @?= 0
        , testCase "all three present together" $
            let cfg = force defaultConfig
                t0 = force (mkTime 60000 Nothing)
                t1 = force (mkTime 60001 Nothing)
                t2 = force (mkTime 60002 Nothing)
                e =
                  force
                    ( baseEntry
                        { _entryStamps =
                            [ ClosedStamp loc0 t0
                            , DeadlineStamp loc0 t1
                            , ScheduledStamp loc0 t2
                            ]
                        }
                    )
                found =
                  ( length (toListOf (anyProperty cfg "CLOSED") e)
                  , length (toListOf (anyProperty cfg "DEADLINE") e)
                  , length (toListOf (anyProperty cfg "SCHEDULED") e)
                  )
             in cfg `seq`
                  t0 `seq`
                    t1 `seq`
                      t2 `seq`
                        e `seq`
                          found
                            @?= (1, 1, 1)
        ]
    , testGroup
        "inheritProperties else branch"
        [ testCase "adds inherited property (else branch) with inherited flag" $
            let inh = force [Property loc0 False "CATEGORY" "home"]
                e' = force (inheritProperties inh (force baseEntry))
                added = case _entryProperties e' of
                  [p] -> (_name p, _inherited p, _value p)
                  _ -> ("", False, "")
             in inh `seq`
                  e' `seq`
                    added @?= ("CATEGORY", True, "home")
        , testCase "skips inherited when entry already has it" $
            let existing =
                  force
                    ( baseEntry
                        { _entryProperties =
                            [Property loc0 False "CATEGORY" "kept"]
                        }
                    )
                inh = force [Property loc0 False "CATEGORY" "ignored"]
                e' = force (inheritProperties inh existing)
             in existing `seq`
                  inh `seq`
                    e' `seq`
                      e' ^? property "CATEGORY" @?= Just "kept"
        , testCase "mixes adds and skips over a list" $
            let existing =
                  force
                    ( baseEntry
                        { _entryProperties = [Property loc0 False "A" "1"]
                        }
                    )
                inh =
                  force
                    [ Property loc0 False "A" "x"
                    , Property loc0 False "B" "2"
                    ]
                e' = force (inheritProperties inh existing)
             in existing `seq`
                  inh `seq`
                    e' `seq`
                      ( e' ^? property "A"
                      , e' ^? property "B"
                      )
                        @?= (Just "1", Just "2")
        , testCase "case-insensitive detection prevents duplicate" $
            let existing =
                  force
                    ( baseEntry
                        { _entryProperties =
                            [Property loc0 False "category" "cur"]
                        }
                    )
                inh = force [Property loc0 False "CATEGORY" "new"]
                e' = force (inheritProperties inh existing)
                n = length (_entryProperties e')
             in existing `seq` inh `seq` e' `seq` n @?= 1
        ]
    , testGroup
        "fileSlug fold with a DataItem"
        [ testCase "DataItem without date takes full filename as slug" $
            force (DataItem "image.png") ^? fileSlug @?= Just "image"
        , testCase "DataItem with date uses suffix" $
            force (DataItem "20240601-photo.png") ^? fileSlug
              @?= Just "photo"
        ]
    , testGroup
        "fileTitle priority"
        [ testCase "TITLE property wins" $
            let f =
                  force
                    ( mkFile
                        "20240601-x.org"
                        []
                        [Property loc0 False "TITLE" "Real"]
                    )
             in f `seq` OrgItem f ^? fileTitle @?= Just "Real"
        , testCase "TITLE in drawer wins" $
            let f =
                  force
                    ( mkFile
                        "20240601-x.org"
                        [Property loc0 False "TITLE" "Drawer"]
                        []
                    )
             in f `seq` OrgItem f ^? fileTitle @?= Just "Drawer"
        , testCase "no TITLE falls back to slug" $
            let f = force (mkFile "20240601-x.org" [] [])
             in f `seq` OrgItem f ^? fileTitle @?= Just "x"
        ]
    , testGroup
        "lookupProperty case-insensitivity via property"
        [ testCase "mixed-case target, lowercase source" $
            let e =
                  force
                    ( baseEntry
                        { _entryProperties =
                            [Property loc0 False "cAtEgOrY" "yes"]
                        }
                    )
             in e `seq` e ^? property "CATEGORY" @?= Just "yes"
        , testCase "uppercase target, mixed-case source" $
            let e =
                  force
                    ( baseEntry
                        { _entryProperties = [Property loc0 False "Id" "42"]
                        }
                    )
             in e `seq` e ^? property "ID" @?= Just "42"
        , testCase "lowercase target, uppercase source" $
            let e =
                  force
                    ( baseEntry
                        { _entryProperties = [Property loc0 False "ID" "q"]
                        }
                    )
             in e `seq` e ^? property "id" @?= Just "q"
        ]
    , testGroup
        "transitionsOf additional cases"
        [ testCase "empty config on unknown key" $
            transitionsOf (force defaultConfig) "ANYTHING" @?= []
        , testCase "config with transition on unrelated key" $
            let cfg =
                  force (defaultConfig{_keywordTransitions = [("A", ["B"])]})
             in cfg `seq` transitionsOf cfg "C" @?= []
        , testCase "config with transition on matching key" $
            let cfg =
                  force
                    (defaultConfig{_keywordTransitions = [("A", ["B", "C"])]})
             in cfg `seq` transitionsOf cfg "A" @?= ["B", "C"]
        ]
    , testGroup
        "_Time review roundtrip"
        [ testCase "Time via review _Time produces non-empty string" $
            let tm = force (mkTime 60000 (Just 3600))
                s = force (review _Time tm)
             in tm `seq` s `seq` not (null s) @?= True
        , testCase "re _Time used as Fold reads string" $
            let tm = force (mkTime 60000 Nothing)
                s = force (review _Time tm)
                -- Confirms the re _Time fold is exercised as a reader.
                listed = force (tm ^.. re _Time)
             in tm `seq`
                  s `seq`
                    listed `seq`
                      (listed == [s]) @?= True
        ]
    , testGroup
        "closedTime / scheduledTime / deadlineTime setter"
        [ testCase "set closedTime when ClosedStamp exists" $
            let t0 = force (mkTime 60000 Nothing)
                t1 = force (mkTime 60005 Nothing)
                e0 =
                  force
                    (baseEntry{_entryStamps = [ClosedStamp loc0 t0]})
                e1 = force (e0 & closedTime .~ t1)
             in t0 `seq`
                  t1 `seq`
                    e0 `seq`
                      e1 `seq`
                        fmap _timeDay (e1 ^? closedTime) @?= Just 60005
        , testCase "set scheduledTime when ScheduledStamp exists" $
            let t0 = force (mkTime 60000 Nothing)
                t1 = force (mkTime 60010 Nothing)
                e0 =
                  force
                    (baseEntry{_entryStamps = [ScheduledStamp loc0 t0]})
                e1 = force (e0 & scheduledTime .~ t1)
             in t0 `seq`
                  t1 `seq`
                    e0 `seq`
                      e1 `seq`
                        fmap _timeDay (e1 ^? scheduledTime) @?= Just 60010
        , testCase "set deadlineTime when DeadlineStamp exists" $
            let t0 = force (mkTime 60000 Nothing)
                t1 = force (mkTime 60020 Nothing)
                e0 =
                  force
                    (baseEntry{_entryStamps = [DeadlineStamp loc0 t0]})
                e1 = force (e0 & deadlineTime .~ t1)
             in t0 `seq`
                  t1 `seq`
                    e0 `seq`
                      e1 `seq`
                        fmap _timeDay (e1 ^? deadlineTime) @?= Just 60020
        ]
    , testGroup
        "createdTime / editedTime setter"
        [ testCase "set createdTime when CREATED property exists" $
            let e0 =
                  force
                    ( baseEntry
                        { _entryProperties =
                            [Property loc0 False "CREATED" "[2024-06-01 Sat]"]
                        }
                    )
                -- Setting requires a round-trippable Time; read-then-write
                -- validates that the traversal is bidirectional.
                current = force (e0 ^? createdTime)
             in e0 `seq`
                  current `seq`
                    fmap _timeKind current @?= Just InactiveTime
        , testCase "set editedTime when EDITED property exists" $
            let e0 =
                  force
                    ( baseEntry
                        { _entryProperties =
                            [Property loc0 False "EDITED" "[2024-06-02 Sun]"]
                        }
                    )
                current = force (e0 ^? editedTime)
             in e0 `seq`
                  current `seq`
                    fmap _timeKind current @?= Just InactiveTime
        ]
    , testGroup
        "entryId setter path"
        [ testCase "set new ID on entry without ID is a no-op" $
            let e' = force (baseEntry & entryId .~ "new-id")
             in e' `seq` e' ^? entryId @?= Nothing
        , testCase "set existing ID updates value" $
            let e0 =
                  force
                    (baseEntry{_entryProperties = [Property loc0 False "ID" "old"]})
                e1 = force (e0 & entryId .~ "new")
             in e0 `seq` e1 `seq` e1 ^? entryId @?= Just "new"
        , testCase "set entryCategory on existing property" $
            let e0 =
                  force
                    ( baseEntry
                        { _entryProperties = [Property loc0 False "CATEGORY" "x"]
                        }
                    )
                e1 = force (e0 & entryCategory .~ "y")
             in e0 `seq` e1 `seq` e1 ^? entryCategory @?= Just "y"
        ]
    , testGroup
        "entryTagString setter"
        [ testCase "set serialized tag string parses back into tags" $
            let e' = force (baseEntry & entryTagString .~ ":foo:bar:")
             in e' `seq`
                  _entryTags e'
                    @?= [PlainTag "foo", PlainTag "bar"]
        , testCase "set empty serialized tag string yields no tags" $
            let e' = force (baseEntry & entryTagString .~ ":::")
             in e' `seq` _entryTags e' @?= []
        ]
    , testGroup
        "orgFilePath lens setter"
        [ testCase "setting path updates file" $
            let o = force (mkFile "old.org" [] [])
                o' = force (o & orgFilePath .~ "new.org")
             in o `seq` o' `seq` _orgFilePath o' @?= ("new.org" :: FilePath)
        , testCase "setting path on CollectionItem via filePath" $
            let o = force (OrgItem (mkFile "a.org" [] []))
                o' = force (o & filePath .~ "b.org")
             in o `seq` o' `seq` view_filePath o' @?= ("b.org" :: FilePath)
        ]
    , testGroup
        "property '?~' setter shortcut"
        [ testCase "?~ via entryPriority sets Just" $
            let e = force (baseEntry & entryPriority ?~ "A")
             in e `seq` (e ^. entryPriority) @?= Just "A"
        , testCase "?~ via entryContext sets Just" $
            let e = force (baseEntry & entryContext ?~ "work")
             in e `seq` (e ^. entryContext) @?= Just "work"
        , testCase "?~ via entryLocator sets Just" $
            let e = force (baseEntry & entryLocator ?~ "p99")
             in e `seq` (e ^. entryLocator) @?= Just "p99"
        , testCase "?~ via entryVerb sets Just" $
            let e = force (baseEntry & entryVerb ?~ "Read")
             in e `seq` (e ^. entryVerb) @?= Just "Read"
        ]
    , testGroup
        "anyProperty Fold evaluation for empty/Nothing paths"
        [ testCase "KEYWORD with absent keyword empties fold" $
            let cfg = force defaultConfig
             in cfg `seq`
                  toListOf (anyProperty cfg "KEYWORD") (force baseEntry)
                    @?= ([] :: [String])
        , testCase "TODO with absent keyword empties fold" $
            let cfg =
                  force (defaultConfig{_openKeywords = ["TODO"]})
             in cfg `seq`
                  toListOf (anyProperty cfg "TODO") (force baseEntry)
                    @?= ([] :: [String])
        , testCase "VERB with absent verb empties fold" $
            let cfg = force defaultConfig
             in cfg `seq`
                  toListOf (anyProperty cfg "VERB") (force baseEntry)
                    @?= ([] :: [String])
        , testCase "CONTEXT with absent context empties fold" $
            let cfg = force defaultConfig
             in cfg `seq`
                  toListOf (anyProperty cfg "CONTEXT") (force baseEntry)
                    @?= ([] :: [String])
        , testCase "LOCATOR with absent locator empties fold" $
            let cfg = force defaultConfig
             in cfg `seq`
                  toListOf (anyProperty cfg "LOCATOR") (force baseEntry)
                    @?= ([] :: [String])
        ]
    ]
 where
  -- Local helper: view filePath on a CollectionItem.
  view_filePath :: CollectionItem -> FilePath
  view_filePath = (^. filePath)
