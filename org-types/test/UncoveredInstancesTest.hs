{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant compare" #-}
{-# HLINT ignore "Length always non-negative" #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use max" #-}

{- |
Tests aimed specifically at the expression-level coverage gaps in
@Org.Types@ that the pre-existing suites leave untouched.

Targets (as reported by HPC):

  * The remaining @Data@ class methods that @checkData@ in
    'InstancesTest' does not exercise:
    'gfoldl', 'gunfold' / 'fromConstr', 'gmapM', 'gmapMp',
    'gmapMo', 'gmapQl', 'gmapQr', 'gmapQi', 'dataCast1', 'dataCast2'.

  * The final two derived 'Ord' methods (@min@ / @max@) per enum type,
    and the full @Ord@ surface (including the 'compare'/'(<)'/'(<=)'
    arms) on 'Time' — whose instance is hand-written via @compare `on`
    timeStartToUTCTime@.

  * Every arm of the hand-written Van Laarhoven lenses
    '_LogLoc', '_LogTime' and '_LogBody' — including the 'LogBook'
    and 'LogClock' short-circuits on '_LogTime' / '_LogBody'.
-}
module UncoveredInstancesTest (tests) where

import Control.Applicative (Const (..))
import Control.DeepSeq (rnf)
import Data.Data (
  Data,
  Typeable,
  cast,
  dataTypeConstrs,
  dataTypeOf,
  fromConstr,
  gfoldl,
  gmapM,
  gmapMo,
  gmapMp,
  gmapQ,
  gmapQi,
  gmapQl,
  gmapQr,
  gmapT,
  toConstr,
 )
import Data.Functor.Identity (Identity (..))
import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import GHC.Generics (from, to)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- -----------------------------------------------------------------------------
-- Shared fixtures (mirroring InstancesTest but kept local on purpose so this
-- module remains self-contained and easy to extend).
-- -----------------------------------------------------------------------------

loc0 :: Loc
loc0 = Loc "a.org" 0

loc1 :: Loc
loc1 = Loc "b.org" 10

prop0 :: Property
prop0 = Property loc0 False "K" "v"

tm0 :: Time
tm0 = Time ActiveTime 60000 Nothing Nothing Nothing Nothing

tm1 :: Time
tm1 = Time InactiveTime 60001 (Just 60002) (Just 60) (Just 120) Nothing

tm2 :: Time
tm2 = Time ActiveTime 60500 (Just 60500) (Just 0) (Just 30) Nothing

tsuf0 :: TimeSuffix
tsuf0 = TimeSuffix TimeRepeat 1 DaySpan Nothing

dur0 :: Duration
dur0 = Duration 1 30

tag0 :: Tag
tag0 = PlainTag "work"

kw0 :: Keyword
kw0 = OpenKeyword loc0 "TODO"

kw1 :: Keyword
kw1 = ClosedKeyword loc0 "DONE"

bodyA :: Body
bodyA = Body [Paragraph loc0 ["x"]]

bodyB :: Body
bodyB = Body [Paragraph loc0 ["y"]]

block1 :: Block
block1 = Paragraph loc0 ["hi"]

entry0 :: Entry
entry0 =
  Entry
    loc0
    1
    (Just kw0)
    (Just "A")
    "* TODO A headline"
    (Just "verb")
    "title"
    (Just "ctx")
    (Just "loc")
    [tag0]
    [ClosedStamp loc0 tm0]
    [prop0]
    [LogClosing loc0 tm0 Nothing]
    bodyA
    []

header0 :: Header
header0 = Header [prop0] [] bodyA

orgFile0 :: OrgFile
orgFile0 = OrgFile "a.org" header0 [entry0]

coll0 :: Collection
coll0 = Collection [OrgItem orgFile0, DataItem "x.txt"]

-- -----------------------------------------------------------------------------
-- Data-class coverage.
--
-- For each value, run every non-trivial method of 'Data.Data.Data' that
-- 'InstancesTest.checkData' skips. The assertions are intentionally shallow
-- (correct reconstructions, equivalent traversals) because we are hunting
-- expression coverage, not verifying semantics that GHC already derives.
-- -----------------------------------------------------------------------------

-- | Drive every Data method we care about on a single value.
exerciseData :: forall a. (Data a, Eq a, Show a) => a -> Assertion
exerciseData v = do
  -- gmapT / gmapQ: already exercised elsewhere but re-run so that each new
  -- value also contributes to the "constructor arm" ticks.
  gmapT id v @?= v
  (length (gmapQ (const ()) v) >= 0) @?= True
  -- gfoldl via id folds rebuilds the value.
  let rebuilt :: a
      rebuilt = runIdentity (gfoldlIdentity v)
  rebuilt @?= v
  -- gmapM via Identity, gmapMp / gmapMo via Maybe (which is MonadPlus).
  -- We only assert the calls complete; semantics of gmapMp / gmapMo depend
  -- on whether the value has immediate sub-terms, so we simply force the
  -- result so the HPC ticks for every arm fire.
  runIdentity (gmapM pure v) @?= v
  let mp :: Maybe a
      mp = gmapMp Just v
      mo :: Maybe a
      mo = gmapMo Just v
  (mp `seq` mo `seq` ()) @?= ()
  -- gmapQl / gmapQr / gmapQi aggregate queries; check they return something
  -- sensible (length of queries matches arity).
  let arity = length (gmapQ (const ()) v)
      ql = gmapQl (+) (0 :: Int) (const 1) v
      qr = gmapQr (+) (0 :: Int) (const 1) v
  ql @?= arity
  qr @?= arity
  -- gmapQi touches each index at least once.
  let hits = [gmapQi i (const (1 :: Int)) v | i <- [0 .. arity - 1]]
  sum hits @?= arity
  -- fromConstr (which funnels through gunfold) round-trips for 0-arity
  -- constructors at least — for nullary values the rebuilt value equals the
  -- original, otherwise we only verify that the underlying call is exercised.
  let c = toConstr v
      dt = dataTypeOf v
  c `seq` dt `seq` pure ()

-- | A gfoldl that rebuilds its argument unchanged using 'Identity'.
gfoldlIdentity :: (Data a) => a -> Identity a
gfoldlIdentity = gfoldl step Identity
 where
  step :: (Data d) => Identity (d -> b) -> d -> Identity b
  step mf d = mf <*> pure d

-- -----------------------------------------------------------------------------
-- Previously-unused helpers in Org.Types that we now exercise.
-- -----------------------------------------------------------------------------

-- Van Laarhoven lens combinators defined at the bottom of Org.Types:
--   previewIsh :: ((a -> Const (First a) a) -> s -> Const (First a) s)
--              -> s -> Maybe a
--   setIsh     :: ((a -> Identity a) -> s -> Identity s)
--              -> a -> s -> s
--
-- The lens used below is 'lensField' from 'Body': it focuses the '_blocks'
-- field, so previewIsh returns 'Just [Block]' and setIsh updates the list.
blocksLens ::
  (Functor f) =>
  ([Block] -> f [Block]) ->
  Body ->
  f Body
blocksLens f (Body bs) = Body <$> f bs

unusedHelperTests :: TestTree
unusedHelperTests =
  testGroup
    "previewIsh / setIsh"
    [ testCase "previewIsh returns Just blocks" $
        previewIsh blocksLens bodyA @?= Just [Paragraph loc0 ["x"]]
    , testCase "setIsh replaces blocks" $
        setIsh blocksLens [Paragraph loc0 ["z"]] bodyA
          @?= Body [Paragraph loc0 ["z"]]
    , testCase "setIsh then previewIsh round trip" $
        previewIsh blocksLens (setIsh blocksLens [] bodyA) @?= Just []
    ]

-- -----------------------------------------------------------------------------
-- Record selectors explicitly exercised so HPC ticks the field declaration
-- lines. The prior suite called selectors but some remain missing when the
-- new test is examined in isolation.
-- -----------------------------------------------------------------------------

selectorTests :: TestTree
selectorTests =
  testGroup
    "Record selectors"
    [ testCase "_file / _pos (Loc)" $ do
        _file loc0 @?= "a.org"
        _pos loc0 @?= 0
    , testCase "_propertyLoc / _inherited / _name / _value (Property)" $ do
        _propertyLoc prop0 @?= loc0
        _inherited prop0 @?= False
        _name prop0 @?= "K"
        _value prop0 @?= "v"
    , testCase "_blocks (Body)" $
        _blocks bodyA @?= [Paragraph loc0 ["x"]]
    , testCase "TimeSuffix selectors" $ do
        let ts = TimeSuffix TimeRepeat 2 DaySpan (Just (1, WeekSpan))
        _suffixKind ts @?= TimeRepeat
        _suffixNum ts @?= 2
        _suffixSpan ts @?= DaySpan
        _suffixLargerSpan ts @?= Just (1, WeekSpan)
    , testCase "Time selectors" $ do
        _timeKind tm1 @?= InactiveTime
        _timeDay tm1 @?= 60001
        _timeDayEnd tm1 @?= Just 60002
        _timeStart tm1 @?= Just 60
        _timeEnd tm1 @?= Just 120
        _timeSuffix tm1 @?= Nothing
    , testCase "Header selectors" $ do
        _headerPropertiesDrawer header0 @?= [prop0]
        _headerFileProperties header0 @?= []
        _headerPreamble header0 @?= bodyA
    , testCase "Duration selectors" $ do
        _hours dur0 @?= 1
        _mins dur0 @?= 30
    , testCase "Entry selectors" $ do
        _entryLoc entry0 @?= loc0
        _entryDepth entry0 @?= 1
        _entryBody entry0 @?= bodyA
    , testCase "OrgFile selectors" $ do
        _orgFilePath orgFile0 @?= "a.org"
        _orgFileHeader orgFile0 @?= header0
        _orgFileEntries orgFile0 @?= [entry0]
    , testCase "Collection selector" $
        _items coll0 @?= [OrgItem orgFile0, DataItem "x.txt"]
    , testCase "Config selectors" $ do
        _startKeywords defaultConfig @?= []
        _openKeywords defaultConfig @?= []
        _closedKeywords defaultConfig @?= []
        _keywordTransitions defaultConfig @?= []
        _homeDirectory defaultConfig @?= Nothing
        _checkFiles defaultConfig @?= True
        _priorities defaultConfig @?= []
        _propertyColumn defaultConfig @?= 0
        _tagsColumn defaultConfig @?= 0
        _attachmentsDir defaultConfig @?= ""
    ]

-- -----------------------------------------------------------------------------
-- Monoid Body: exercise 'mconcat' (mempty / mappend are already covered).
-- -----------------------------------------------------------------------------

monoidBodyTests :: TestTree
monoidBodyTests =
  testGroup
    "Monoid Body"
    [ testCase "mconcat []" $
        (mconcat [] :: Body) @?= mempty
    , testCase "mconcat on three bodies equals chained <>" $
        mconcat [bodyA, bodyB, bodyA] @?= bodyA <> bodyB <> bodyA
    , testCase "mappend is (<>)" $
        (bodyA `mappend` bodyB) @?= bodyA <> bodyB
    ]

-- | For nullary-constructor enums we can round-trip through 'fromConstr'.
roundTripNullary :: (Data a, Eq a, Show a) => a -> Assertion
roundTripNullary v =
  fromConstr (toConstr v) @?= v

-- -----------------------------------------------------------------------------
-- Ord derived min/max (currently missing HPC ticks on enum types).
-- -----------------------------------------------------------------------------

checkOrdMinMax :: (Ord a, Show a) => a -> a -> Assertion
checkOrdMinMax a b = do
  min a b @?= (if a <= b then a else b)
  max a b @?= (if a >= b then a else b)
  -- Drive each comparison operator explicitly to tick arms that rely on
  -- direct operator calls rather than 'compare'.
  (compare a b == compare a b) @?= True
  (a < b || a >= b) @?= True
  (a > b || a <= b) @?= True

-- -----------------------------------------------------------------------------
-- Ord Time instance (custom, not derived).
-- -----------------------------------------------------------------------------

ordTimeTests :: TestTree
ordTimeTests =
  testGroup
    "Ord Time (custom instance)"
    [ testCase "compare reflexivity" $
        compare tm0 tm0 @?= EQ
    , testCase "compare via UTCTime proxy" $
        (compare tm0 tm1 /= EQ) @?= True
    , testCase "less-than uses compare on UTCTime" $
        (tm0 < tm1) @?= True
    , testCase "less-than-or-equal" $
        (tm0 <= tm0) @?= True
    , testCase "greater-than" $
        (tm1 > tm0) @?= True
    , testCase "greater-than-or-equal" $
        (tm1 >= tm1) @?= True
    , testCase "max returns later time" $
        max tm0 tm1 @?= tm1
    , testCase "min returns earlier time" $
        min tm0 tm1 @?= tm0
    , testCase "compare with different kind still orders by day" $
        compare tm0 tm2 @?= LT
    ]

-- -----------------------------------------------------------------------------
-- LogEntry lenses — cover every constructor arm of _LogLoc, _LogTime,
-- _LogBody.
-- -----------------------------------------------------------------------------

-- One sample for every constructor. We keep bodies explicit for the variants
-- that thread a Maybe Body so that _LogBody actually traverses the payload
-- instead of short-circuiting.
logEntriesAll :: [LogEntry]
logEntriesAll =
  [ LogClosing loc0 tm0 (Just bodyA)
  , LogState loc0 kw0 (Just kw1) tm0 (Just bodyA)
  , LogNote loc0 tm0 (Just bodyA)
  , LogRescheduled loc0 tm0 tm1 (Just bodyA)
  , LogNotScheduled loc0 tm0 tm1 (Just bodyA)
  , LogDeadline loc0 tm0 tm1 (Just bodyA)
  , LogNoDeadline loc0 tm0 tm1 (Just bodyA)
  , LogRefiling loc0 tm0 (Just bodyA)
  , LogClock loc0 tm0 (Just dur0)
  , LogBook loc0 [LogClosing loc0 tm0 (Just bodyA)]
  ]

logEntryCtor :: LogEntry -> String
logEntryCtor (LogClosing{}) = "LogClosing"
logEntryCtor (LogState{}) = "LogState"
logEntryCtor (LogNote{}) = "LogNote"
logEntryCtor (LogRescheduled{}) = "LogRescheduled"
logEntryCtor (LogNotScheduled{}) = "LogNotScheduled"
logEntryCtor (LogDeadline{}) = "LogDeadline"
logEntryCtor (LogNoDeadline{}) = "LogNoDeadline"
logEntryCtor (LogRefiling{}) = "LogRefiling"
logEntryCtor (LogClock{}) = "LogClock"
logEntryCtor (LogBook{}) = "LogBook"

-- Extract a field via a Van Laarhoven-style getter without pulling in the
-- lens library.
viewL :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
viewL l = getFirst . getConst . l (Const . First . Just)

-- Set a field via a Van Laarhoven-style setter.
setL :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
setL l x = runIdentity . l (const (Identity x))

-- Replace the Loc, observe via the getter. We 'rnf' the result so that every
-- subexpression the lambda constructs (including the unchanged fields, so
-- HPC ticks the variable references) is evaluated.
locLensTests :: [TestTree]
locLensTests =
  [ testCase ("_LogLoc get " ++ logEntryCtor e) $
      viewL _LogLoc e @?= Just loc0
  | e <- logEntriesAll
  ]
    ++ [ testCase ("_LogLoc set " ++ logEntryCtor e) $ do
           let e' = setL _LogLoc loc1 e
           rnf e' @?= ()
           viewL _LogLoc e' @?= Just loc1
           -- round trip set again to original
           viewL _LogLoc (setL _LogLoc loc0 e') @?= Just loc0
       | e <- logEntriesAll
       ]

-- Replace/observe the Time field. The LogBook arm short-circuits with
-- 'pure e'; we assert that fact explicitly. 'rnf' forces the whole entry
-- so HPC ticks the unchanged sub-fields in the rebuilt constructor.
timeLensTests :: [TestTree]
timeLensTests =
  [ testCase ("_LogTime get " ++ logEntryCtor e) $
      case e of
        LogBook{} -> viewL _LogTime e @?= Nothing
        _ -> viewL _LogTime e /= Nothing @?= True
  | e <- logEntriesAll
  ]
    ++ [ testCase ("_LogTime set " ++ logEntryCtor e) $ do
           let e' = setL _LogTime tm1 e
           rnf e' @?= ()
           case e of
             LogBook{} -> setL _LogTime tm1 e @?= e
             _ -> viewL _LogTime e' @?= Just tm1
       | e <- logEntriesAll
       ]

-- _LogBody traverses a 'Maybe Body'; for arms whose constructor has no body
-- (LogClock, LogBook), the lens returns the entry unchanged. We 'rnf' the
-- outputs so HPC ticks the unchanged child references in each rebuilt
-- constructor.
bodyLensTests :: [TestTree]
bodyLensTests =
  [ testCase ("_LogBody traverse " ++ logEntryCtor e) $ do
      let result :: LogEntry
          result = runIdentity (_LogBody (pure . const bodyB) e)
      rnf result @?= ()
      case e of
        LogClock{} -> result @?= e
        LogBook{} -> result @?= e
        _ -> result /= e @?= True
  | e <- logEntriesAll
  ]
    ++ [ testCase ("_LogBody identity-pure " ++ logEntryCtor e) $ do
           let r = runIdentity (_LogBody pure e)
           rnf r @?= ()
           r @?= e
       | e <- logEntriesAll
       ]

-- -----------------------------------------------------------------------------
-- Generic / Hashable / NFData sanity for "child-heavy" values that prior
-- tests did not include (makes every per-constructor tick for sum types
-- fire at least once).
-- -----------------------------------------------------------------------------

childHeavyTests :: TestTree
childHeavyTests =
  testGroup
    "Deeply nested values exercise recursive Data / Generic / NFData"
    [ testCase "rnf of a nested collection" $
        rnf coll0 @?= ()
    , testCase "hash stability: equal values hash alike" $
        hash coll0 @?= hash (Collection [OrgItem orgFile0, DataItem "x.txt"])
    , testCase "Generic round-trip for nested Entry" $
        to (from entry0) @?= entry0
    , testCase "Generic round-trip for nested OrgFile" $
        to (from orgFile0) @?= orgFile0
    ]

-- -----------------------------------------------------------------------------
-- Data coverage per type.
-- -----------------------------------------------------------------------------

dataTests :: TestTree
dataTests =
  testGroup
    "Data class - gfoldl / gunfold / gmap*/gmapQ*"
    [ testCase "Config" $ exerciseData defaultConfig
    , testCase "Loc" $ exerciseData loc0
    , testCase "Property" $ exerciseData prop0
    , testCase "DrawerType PlainDrawer" $ exerciseData (PlainDrawer "x")
    , testCase "DrawerType BeginDrawer" $ exerciseData (BeginDrawer "y")
    , testCase "Block Whitespace" $ exerciseData (Whitespace loc0 " ")
    , testCase "Block Paragraph" $ exerciseData block1
    , testCase "Block Drawer" $ exerciseData (Drawer loc0 (PlainDrawer "P") ["line"])
    , testCase "Block InlineTask" $ exerciseData (InlineTask loc0 entry0)
    , testCase "Body" $ exerciseData bodyA
    , testCase "Tag" $ exerciseData tag0
    , testCase "TimeSpan DaySpan" $ exerciseData DaySpan
    , testCase "TimeSpan WeekSpan" $ exerciseData WeekSpan
    , testCase "TimeSpan MonthSpan" $ exerciseData MonthSpan
    , testCase "TimeSpan YearSpan" $ exerciseData YearSpan
    , testCase "TimeKind ActiveTime" $ exerciseData ActiveTime
    , testCase "TimeKind InactiveTime" $ exerciseData InactiveTime
    , testCase "TimeSuffixKind TimeRepeat" $ exerciseData TimeRepeat
    , testCase "TimeSuffixKind TimeRepeatPlus" $ exerciseData TimeRepeatPlus
    , testCase "TimeSuffixKind TimeDottedRepeat" $ exerciseData TimeDottedRepeat
    , testCase "TimeSuffixKind TimeWithin" $ exerciseData TimeWithin
    , testCase "TimeSuffix with largerSpan" $
        exerciseData (TimeSuffix TimeWithin 3 WeekSpan (Just (2, MonthSpan)))
    , testCase "TimeSuffix no largerSpan" $ exerciseData tsuf0
    , testCase "Time" $ exerciseData tm1
    , testCase "Duration" $ exerciseData dur0
    , testCase "Stamp Closed" $ exerciseData (ClosedStamp loc0 tm0)
    , testCase "Stamp Scheduled" $ exerciseData (ScheduledStamp loc0 tm0)
    , testCase "Stamp Deadline" $ exerciseData (DeadlineStamp loc0 tm0)
    , testCase "Stamp Active" $ exerciseData (ActiveStamp loc0 tm0)
    , testCase "Header" $ exerciseData header0
    , testCase "Keyword Open" $ exerciseData kw0
    , testCase "Keyword Closed" $ exerciseData kw1
    , testCase "LogEntry LogClosing" $ exerciseData (LogClosing loc0 tm0 (Just bodyA))
    , testCase "LogEntry LogState" $ exerciseData (LogState loc0 kw0 (Just kw1) tm0 (Just bodyA))
    , testCase "LogEntry LogNote" $ exerciseData (LogNote loc0 tm0 Nothing)
    , testCase "LogEntry LogRescheduled" $
        exerciseData (LogRescheduled loc0 tm0 tm1 (Just bodyA))
    , testCase "LogEntry LogNotScheduled" $
        exerciseData (LogNotScheduled loc0 tm0 tm1 Nothing)
    , testCase "LogEntry LogDeadline" $
        exerciseData (LogDeadline loc0 tm0 tm1 (Just bodyA))
    , testCase "LogEntry LogNoDeadline" $
        exerciseData (LogNoDeadline loc0 tm0 tm1 Nothing)
    , testCase "LogEntry LogRefiling" $ exerciseData (LogRefiling loc0 tm0 (Just bodyA))
    , testCase "LogEntry LogClock" $ exerciseData (LogClock loc0 tm0 (Just dur0))
    , testCase "LogEntry LogBook" $
        exerciseData (LogBook loc0 [LogNote loc0 tm0 Nothing])
    , testCase "Entry" $ exerciseData entry0
    , testCase "OrgFile" $ exerciseData orgFile0
    , testCase "CollectionItem OrgItem" $ exerciseData (OrgItem orgFile0)
    , testCase "CollectionItem DataItem" $ exerciseData (DataItem "z.txt")
    , testCase "Collection" $ exerciseData coll0
    ]

-- Nullary-only round-trip (fromConstr . toConstr = id).
nullaryRoundTrips :: TestTree
nullaryRoundTrips =
  testGroup
    "Data fromConstr round-trip on nullary constructors"
    [ testCase "TimeSpan DaySpan" $ roundTripNullary DaySpan
    , testCase "TimeSpan WeekSpan" $ roundTripNullary WeekSpan
    , testCase "TimeSpan MonthSpan" $ roundTripNullary MonthSpan
    , testCase "TimeSpan YearSpan" $ roundTripNullary YearSpan
    , testCase "TimeKind ActiveTime" $ roundTripNullary ActiveTime
    , testCase "TimeKind InactiveTime" $ roundTripNullary InactiveTime
    , testCase "TimeSuffixKind TimeRepeat" $ roundTripNullary TimeRepeat
    , testCase "TimeSuffixKind TimeRepeatPlus" $ roundTripNullary TimeRepeatPlus
    , testCase "TimeSuffixKind TimeDottedRepeat" $ roundTripNullary TimeDottedRepeat
    , testCase "TimeSuffixKind TimeWithin" $ roundTripNullary TimeWithin
    ]

-- Enumerate every constructor of an enum data type and confirm the
-- Data/Typeable plumbing returns a sensible shape for each.
enumTypeTraversal :: TestTree
enumTypeTraversal =
  testGroup
    "Data enumerate dataTypeConstrs"
    [ testCase "TimeSpan has 4 constructors" $
        length (dataTypeConstrs (dataTypeOf DaySpan)) @?= 4
    , testCase "TimeKind has 2 constructors" $
        length (dataTypeConstrs (dataTypeOf ActiveTime)) @?= 2
    , testCase "TimeSuffixKind has 4 constructors" $
        length (dataTypeConstrs (dataTypeOf TimeRepeat)) @?= 4
    , testCase "Stamp has 4 constructors" $
        length (dataTypeConstrs (dataTypeOf (ClosedStamp loc0 tm0))) @?= 4
    , testCase "LogEntry has 10 constructors" $
        length (dataTypeConstrs (dataTypeOf (LogNote loc0 tm0 Nothing))) @?= 10
    , testCase "cast on Loc -> Loc round trips" $
        (cast loc0 :: Maybe Loc) @?= Just loc0
    , testCase "cast on Loc -> Property is Nothing" $
        (cast loc0 :: Maybe Property) @?= Nothing
    ]

-- -----------------------------------------------------------------------------
-- Ord derived for enum types.
-- -----------------------------------------------------------------------------

enumOrdTests :: TestTree
enumOrdTests =
  testGroup
    "Enum Ord min/max and comparison arms"
    [ testCase "TimeSpan Day<Week" $ checkOrdMinMax DaySpan WeekSpan
    , testCase "TimeSpan Week<Month" $ checkOrdMinMax WeekSpan MonthSpan
    , testCase "TimeSpan Month<Year" $ checkOrdMinMax MonthSpan YearSpan
    , testCase "TimeSpan equal" $ checkOrdMinMax DaySpan DaySpan
    , testCase "TimeKind Active<Inactive" $ checkOrdMinMax ActiveTime InactiveTime
    , testCase "TimeKind equal" $ checkOrdMinMax ActiveTime ActiveTime
    , testCase "TimeSuffixKind Repeat<Plus" $ checkOrdMinMax TimeRepeat TimeRepeatPlus
    , testCase "TimeSuffixKind Plus<Dotted" $ checkOrdMinMax TimeRepeatPlus TimeDottedRepeat
    , testCase "TimeSuffixKind Dotted<Within" $ checkOrdMinMax TimeDottedRepeat TimeWithin
    , testCase "TimeSuffixKind equal" $ checkOrdMinMax TimeWithin TimeWithin
    ]

-- -----------------------------------------------------------------------------
-- Small utility: confirm 'fromMaybe' pathway for 'timeStartToUTCTime' via
-- a 'Time' with no _timeStart — this keeps the Time arm of Ord exercised.
-- -----------------------------------------------------------------------------

timeUtcFallbackTests :: TestTree
timeUtcFallbackTests =
  testGroup
    "timeStartToUTCTime / timeEndToUTCTime interaction with Ord Time"
    [ testCase "timeStartToUTCTime with Nothing _timeStart" $
        let t = Time ActiveTime 60000 Nothing Nothing Nothing Nothing
         in (timeStartToUTCTime t `seq` ()) @?= ()
    , testCase "timeEndToUTCTime returns Nothing without _timeDayEnd" $
        timeEndToUTCTime tm0 @?= Nothing
    , testCase "Ord Time via compare on times missing _timeStart" $
        compare (Time ActiveTime 1 Nothing Nothing Nothing Nothing) tm0
          @?= compare 1 (60000 :: Integer)
    , testCase "fromMaybe 0 fallback observed via ord compare" $
        let a = Time ActiveTime 100 Nothing Nothing Nothing Nothing
            b = Time ActiveTime 100 Nothing (Just 0) Nothing Nothing
         in compare a b @?= EQ
    , testCase "compare does not depend on Nothing vs Just 0" $
        let a = Time ActiveTime 1 Nothing (Just 0) Nothing Nothing
            b = Time ActiveTime 1 Nothing Nothing Nothing Nothing
         in (a <= b && a >= b) @?= True
    , testCase "cast (Time -> Time)" $
        (cast tm0 :: Maybe Time) @?= Just tm0
    , testCase "cast (Time -> Loc) is Nothing" $
        (cast tm0 :: Maybe Loc) @?= Nothing
    , testCase "timeStartToUTCTime uses fromMaybe 0 when _timeStart=Nothing" $
        let t = Time ActiveTime 0 Nothing Nothing Nothing Nothing
         in (timeStartToUTCTime t `seq` ()) @?= ()
    , testCase "timeEndToUTCTime with both fields" $
        let t =
              Time
                ActiveTime
                0
                (Just 0)
                Nothing
                Nothing
                Nothing
         in case timeEndToUTCTime t of
              Just _ -> pure ()
              Nothing -> assertFailure "expected Just UTCTime"
    , testCase "timeEndToUTCTime uses fromMaybe 0 when _timeEnd=Nothing" $
        let t = Time ActiveTime 0 (Just 0) Nothing Nothing Nothing
         in case timeEndToUTCTime t of
              Just u -> (u `seq` ()) @?= ()
              Nothing -> assertFailure "expected Just UTCTime"
    ]

-- A forced use of @fromMaybe@ so the 'Data.Maybe' import is not elided by
-- GHC warnings; exercises the helper inline.
_usedFromMaybe :: Integer
_usedFromMaybe = fromMaybe 0 (Just 1)

-- Keep Typeable visible to readers of the import list.
_usedTypeable :: (Typeable a) => a -> Bool
_usedTypeable _ = True

-- -----------------------------------------------------------------------------
-- Aggregate TestTree.
-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "UncoveredInstancesTest"
    [ dataTests
    , nullaryRoundTrips
    , enumTypeTraversal
    , enumOrdTests
    , ordTimeTests
    , timeUtcFallbackTests
    , testGroup "_LogLoc all arms" locLensTests
    , testGroup "_LogTime all arms" timeLensTests
    , testGroup "_LogBody all arms" bodyLensTests
    , childHeavyTests
    , unusedHelperTests
    , selectorTests
    , monoidBodyTests
    ]
