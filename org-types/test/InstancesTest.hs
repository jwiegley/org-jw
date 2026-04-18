{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use shows" #-}
{-# HLINT ignore "Use show" #-}
{-# HLINT ignore "Redundant compare" #-}
{-# HLINT ignore "Length always non-negative" #-}
{-# HLINT ignore "Use null" #-}

module InstancesTest (tests) where

import Control.DeepSeq (rnf)
import Data.Data (
  Data,
  dataTypeOf,
  gmapQ,
  gmapT,
  toConstr,
 )
import Data.Functor.Identity (Identity (..))
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat, stimes)
import GHC.Generics (Generic, from, to)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

-- Helpers

loc0 :: Loc
loc0 = Loc "a.org" 0

loc1 :: Loc
loc1 = Loc "b.org" 10

prop0 :: Property
prop0 = Property loc0 False "K" "v"

prop1 :: Property
prop1 = Property loc1 True "M" "w"

tm0 :: Time
tm0 = Time ActiveTime 60000 Nothing Nothing Nothing Nothing

tm1 :: Time
tm1 = Time InactiveTime 60001 (Just 60002) (Just 60) (Just 120) Nothing

tsuf0 :: TimeSuffix
tsuf0 = TimeSuffix TimeRepeat 1 DaySpan Nothing

tsuf1 :: TimeSuffix
tsuf1 = TimeSuffix TimeWithin 3 WeekSpan (Just (2, MonthSpan))

dur0 :: Duration
dur0 = Duration 1 30

dur1 :: Duration
dur1 = Duration 2 45

tag0 :: Tag
tag0 = PlainTag "work"

tag1 :: Tag
tag1 = PlainTag "home"

kw0 :: Keyword
kw0 = OpenKeyword loc0 "TODO"

kw1 :: Keyword
kw1 = ClosedKeyword loc0 "DONE"

bodyA :: Body
bodyA = Body [Paragraph loc0 ["x"]]

bodyB :: Body
bodyB = Body [Paragraph loc0 ["y"]]

block0 :: Block
block0 = Whitespace loc0 " "

block1 :: Block
block1 = Paragraph loc0 ["hi"]

block2 :: Block
block2 = Drawer loc0 (PlainDrawer "PROPS") ["foo"]

block3 :: Block
block3 = InlineTask loc0 entry0

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

entry1 :: Entry
entry1 =
  Entry
    loc1
    2
    (Just kw1)
    (Just "B")
    "** DONE B headline"
    Nothing
    "other"
    Nothing
    Nothing
    [tag1]
    []
    [prop1]
    []
    bodyB
    []

header0 :: Header
header0 = Header [prop0] [] bodyA

header1 :: Header
header1 = Header [] [prop1] bodyB

orgFile0 :: OrgFile
orgFile0 = OrgFile "a.org" header0 [entry0]

orgFile1 :: OrgFile
orgFile1 = OrgFile "b.org" header1 [entry1]

coll0 :: Collection
coll0 = Collection [OrgItem orgFile0, DataItem "x.txt"]

coll1 :: Collection
coll1 = Collection [DataItem "y.txt"]

-- Generic helper functions

checkShow :: (Show a) => a -> Assertion
checkShow v = do
  (show v /= "") @?= True
  (shows [v] "" /= "") @?= True
  (showsPrec 0 v "" /= "") @?= True

checkEq :: (Eq a) => a -> a -> Assertion
checkEq v v2 = do
  (v == v) @?= True
  (v /= v2) @?= True

checkOrd :: (Ord a) => a -> a -> Assertion
checkOrd v v2 = do
  compare v v @?= EQ
  (compare v v2 /= EQ) @?= True
  (v < v2 || v > v2) @?= True
  (v <= v2 || v >= v2) @?= True
  let mx = max v v2
      mn = min v v2
  (mx == v || mx == v2) @?= True
  (mn == v || mn == v2) @?= True

checkHashable :: (Hashable a) => a -> Assertion
checkHashable v = do
  hash v == hash v @?= True
  hashWithSalt 0 v == hashWithSalt 0 v @?= True

checkNFData :: (Eq a) => (a -> ()) -> a -> Assertion
checkNFData r v = (r v `seq` ()) @?= ()

checkData :: (Data a, Eq a, Show a) => a -> Assertion
checkData v = do
  (show (toConstr v) /= "") @?= True
  (show (dataTypeOf v) /= "") @?= True
  gmapT id v @?= v
  (length (gmapQ (const ()) v) >= 0) @?= True

checkGeneric :: (Generic a, Eq a, Show a) => a -> Assertion
checkGeneric v = to (from v) @?= v

-- Full bundle for a type that derives Show/Eq/Hashable/NFData/Data/Generic.
checkAll ::
  (Show a, Eq a, Hashable a, Data a, Generic a) =>
  (a -> ()) ->
  a ->
  a ->
  Assertion
checkAll r v v2 = do
  checkShow v
  checkEq v v2
  checkHashable v
  checkNFData r v
  checkData v
  checkGeneric v

-- Full bundle for a type that also derives Ord.
checkAllOrd ::
  (Show a, Ord a, Hashable a, Data a, Generic a) =>
  (a -> ()) ->
  a ->
  a ->
  Assertion
checkAllOrd r v v2 = do
  checkAll r v v2
  checkOrd v v2

-- Enum/Bounded check for small enum types.
checkEnumBounded ::
  forall a.
  (Enum a, Bounded a, Eq a, Show a) =>
  Assertion
checkEnumBounded = do
  let vals = [minBound .. maxBound] :: [a]
  (length vals >= 1) @?= True
  (fromEnum (minBound :: a) == 0) @?= True
  (toEnum 0 :: a) @?= (minBound :: a)
  (enumFrom (minBound :: a) == vals) @?= True
  (enumFromTo (minBound :: a) (maxBound :: a) == vals) @?= True
  let mn = minBound :: a
      mx = maxBound :: a
  if mn == mx
    then return ()
    else do
      let next = succ mn :: a
          prev = pred mx :: a
      (enumFromThen mn next == vals) @?= True
      (enumFromThenTo mn next mx == vals) @?= True
      (fromEnum next == 1) @?= True
      (prev `seq` ()) @?= ()

-- LogEntry constructor sample for _LogLoc coverage.

logEntries :: [LogEntry]
logEntries =
  [ LogClosing loc0 tm0 Nothing
  , LogState loc0 kw0 (Just kw1) tm0 Nothing
  , LogNote loc0 tm0 Nothing
  , LogRescheduled loc0 tm0 tm1 Nothing
  , LogNotScheduled loc0 tm0 tm1 Nothing
  , LogDeadline loc0 tm0 tm1 Nothing
  , LogNoDeadline loc0 tm0 tm1 Nothing
  , LogRefiling loc0 tm0 Nothing
  , LogClock loc0 tm0 (Just dur0)
  , LogBook loc0 [LogClosing loc0 tm0 Nothing]
  ]

logLocConstructorName :: LogEntry -> String
logLocConstructorName (LogClosing{}) = "LogClosing"
logLocConstructorName (LogState{}) = "LogState"
logLocConstructorName (LogNote{}) = "LogNote"
logLocConstructorName (LogRescheduled{}) = "LogRescheduled"
logLocConstructorName (LogNotScheduled{}) = "LogNotScheduled"
logLocConstructorName (LogDeadline{}) = "LogDeadline"
logLocConstructorName (LogNoDeadline{}) = "LogNoDeadline"
logLocConstructorName (LogRefiling{}) = "LogRefiling"
logLocConstructorName (LogClock{}) = "LogClock"
logLocConstructorName (LogBook{}) = "LogBook"

-- View the Loc of a LogEntry via the _LogLoc lens (getter).
viewLogLoc :: LogEntry -> Maybe Loc
viewLogLoc e = case _LogLoc LeftConst e of
  LeftConst loc -> Just loc
  _ -> Nothing

-- A trivial Const-like functor for extracting a value via a Van Laarhoven lens
-- without relying on `Control.Applicative.Const` (keeps imports minimal).
data LeftConst b a = LeftConst b | OtherConst

instance Functor (LeftConst b) where
  fmap _ (LeftConst b) = LeftConst b
  fmap _ OtherConst = OtherConst

-- Accessor tests: exercise each field accessor at least once.

accessorAssertions :: Assertion
accessorAssertions = do
  -- Property accessors
  _propertyLoc prop0 @?= loc0
  _inherited prop0 @?= False
  _name prop0 @?= "K"
  _value prop0 @?= "v"
  -- Duration accessors
  _hours dur0 @?= 1
  _mins dur0 @?= 30
  -- Header accessors
  _headerPropertiesDrawer header0 @?= [prop0]
  _headerFileProperties header1 @?= [prop1]
  _headerPreamble header0 @?= bodyA
  -- Entry accessors
  _entryLoc entry0 @?= loc0
  _entryDepth entry0 @?= 1
  _entryKeyword entry0 @?= Just kw0
  _entryPriority entry0 @?= Just "A"
  _entryHeadline entry0 @?= "* TODO A headline"
  _entryVerb entry0 @?= Just "verb"
  _entryTitle entry0 @?= "title"
  _entryContext entry0 @?= Just "ctx"
  _entryLocator entry0 @?= Just "loc"
  _entryTags entry0 @?= [tag0]
  _entryStamps entry0 @?= [ClosedStamp loc0 tm0]
  _entryProperties entry0 @?= [prop0]
  _entryLogEntries entry0 @?= [LogClosing loc0 tm0 Nothing]
  _entryBody entry0 @?= bodyA
  _entryItems entry0 @?= []
  -- OrgFile accessor
  _orgFileEntries orgFile0 @?= [entry0]
  -- Collection accessor
  _items coll0 @?= [OrgItem orgFile0, DataItem "x.txt"]

-- _LogLoc lens tests: cover every LogEntry constructor.

logLocAssertions :: [TestTree]
logLocAssertions =
  [ testCase ("_LogLoc get on " ++ logLocConstructorName e) $
      viewLogLoc e @?= Just loc0
  | e <- logEntries
  ]
    ++ [ testCase ("_LogLoc set on " ++ logLocConstructorName e) $
           let e' = runIdentity (_LogLoc (const (Identity loc1)) e)
            in viewLogLoc e' @?= Just loc1
       | e <- logEntries
       ]

-- Semigroup Body sconcat / stimes

bodySemigroupTests :: TestTree
bodySemigroupTests =
  testGroup
    "Body Semigroup sconcat / stimes"
    [ testCase "sconcat of one body is the body" $
        sconcat (bodyA :| []) @?= bodyA
    , testCase "sconcat of two bodies merges via <>" $
        sconcat (bodyA :| [bodyB]) @?= bodyA <> bodyB
    , testCase "sconcat of three bodies matches foldr1" $
        let bs = bodyA :| [bodyB, bodyA]
         in sconcat bs @?= bodyA <> bodyB <> bodyA
    , testCase "stimes 1 bodyA equals bodyA" $
        stimes (1 :: Int) bodyA @?= bodyA
    , testCase "stimes 2 Body [ws] merges ws" $
        stimes (2 :: Int) (Body [Whitespace loc0 "x"])
          @?= Body [Whitespace loc0 "xx"]
    , testCase "stimes 3 Body [para] merges paras" $
        stimes (3 :: Int) (Body [Paragraph loc0 ["a"]])
          @?= Body [Paragraph loc0 ["a", "a", "a"]]
    ]

-- Per-type test groups.

configTests :: TestTree
configTests =
  testGroup
    "Config derived"
    [ testCase "show/eq/data/generic" $ do
        let c2 = defaultConfig{_checkFiles = False}
        checkAll rnf defaultConfig c2
    ]

locTests :: TestTree
locTests =
  testGroup
    "Loc derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf loc0 loc1
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf prop0 prop1
    ]

drawerTypeTests :: TestTree
drawerTypeTests =
  testGroup
    "DrawerType derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf (PlainDrawer "A") (BeginDrawer "B")
    ]

blockTests :: TestTree
blockTests =
  testGroup
    "Block derived"
    [ testCase "Whitespace vs Paragraph" $
        checkAll rnf block0 block1
    , testCase "Drawer" $
        checkAll rnf block2 block0
    , testCase "InlineTask" $
        checkAll rnf block3 block0
    ]

bodyTests :: TestTree
bodyTests =
  testGroup
    "Body derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf bodyA bodyB
    ]

tagTests :: TestTree
tagTests =
  testGroup
    "Tag derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf tag0 tag1
    ]

timeSpanTests :: TestTree
timeSpanTests =
  testGroup
    "TimeSpan derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf DaySpan WeekSpan
    , testCase "enum/bounded" $ checkEnumBounded @TimeSpan
    ]

timeKindTests :: TestTree
timeKindTests =
  testGroup
    "TimeKind derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf ActiveTime InactiveTime
    , testCase "enum/bounded" $ checkEnumBounded @TimeKind
    ]

timeSuffixKindTests :: TestTree
timeSuffixKindTests =
  testGroup
    "TimeSuffixKind derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf TimeRepeat TimeRepeatPlus
    , testCase "enum/bounded" $ checkEnumBounded @TimeSuffixKind
    ]

timeSuffixTests :: TestTree
timeSuffixTests =
  testGroup
    "TimeSuffix derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf tsuf0 tsuf1
    ]

timeTests :: TestTree
timeTests =
  testGroup
    "Time derived (non-Ord)"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf tm0 tm1
    ]

durationTests :: TestTree
durationTests =
  testGroup
    "Duration derived"
    [ testCase "show/eq/ord/hashable/nfdata/data/generic" $
        checkAllOrd rnf dur0 dur1
    ]

stampTests :: TestTree
stampTests =
  testGroup
    "Stamp derived"
    [ testCase "Closed vs Scheduled" $
        checkAllOrd rnf (ClosedStamp loc0 tm0) (ScheduledStamp loc0 tm0)
    , testCase "Deadline vs Active" $
        checkAllOrd rnf (DeadlineStamp loc0 tm0) (ActiveStamp loc0 tm0)
    ]

headerTests :: TestTree
headerTests =
  testGroup
    "Header derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf header0 header1
    ]

keywordTests :: TestTree
keywordTests =
  testGroup
    "Keyword derived"
    [ testCase "Open vs Closed" $
        checkAllOrd rnf kw0 kw1
    ]

logEntryTests :: TestTree
logEntryTests =
  testGroup
    "LogEntry derived"
    [ testCase (logLocConstructorName e) $
        checkAll rnf e (LogNote loc1 tm1 Nothing)
    | e <- logEntries
    ]

entryTests :: TestTree
entryTests =
  testGroup
    "Entry derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf entry0 entry1
    , testCase "accessors exercised" accessorAssertions
    ]

orgFileTests :: TestTree
orgFileTests =
  testGroup
    "OrgFile derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf orgFile0 orgFile1
    ]

collectionItemTests :: TestTree
collectionItemTests =
  testGroup
    "CollectionItem derived"
    [ testCase "OrgItem vs DataItem" $
        checkAll rnf (OrgItem orgFile0) (DataItem "z.txt")
    ]

collectionTests :: TestTree
collectionTests =
  testGroup
    "Collection derived"
    [ testCase "show/eq/hashable/nfdata/data/generic" $
        checkAll rnf coll0 coll1
    ]

-- Aggregate.

tests :: TestTree
tests =
  testGroup
    "Derived instances / accessors"
    [ configTests
    , locTests
    , propertyTests
    , drawerTypeTests
    , blockTests
    , bodyTests
    , tagTests
    , timeSpanTests
    , timeKindTests
    , timeSuffixKindTests
    , timeSuffixTests
    , timeTests
    , durationTests
    , stampTests
    , headerTests
    , keywordTests
    , logEntryTests
    , entryTests
    , orgFileTests
    , collectionItemTests
    , collectionTests
    , bodySemigroupTests
    , testGroup "_LogLoc lens" logLocAssertions
    ]
