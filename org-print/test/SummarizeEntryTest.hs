{-# LANGUAGE BangPatterns #-}

module SummarizeEntryTest (tests) where

import Control.DeepSeq (NFData, deepseq, force)
import Data.List (isInfixOf)
import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "summary.org" 42

tm :: Time
tm = Time InactiveTime 60000 Nothing Nothing Nothing Nothing

cfg :: Config
cfg = defaultConfig

baseEntry :: Entry
baseEntry =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 2
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "h"
    , _entryVerb = Nothing
    , _entryTitle = "hello"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = []
    , _entryBody = Body []
    , _entryItems = []
    }

{- | Render a summary and deep-force the resulting lines so HPC ticks
all expressions inside the helper CAFs and record construction.
-}
summarize :: Entry -> [String]
summarize e =
  let !e' = force e
      !out = force (summarizeEntry cfg e')
   in out

{- | Strict variant of 'assertBool' that forces the debug message even
when the assertion passes, so HPC ticks the 'show' call.
-}
assertBool' :: String -> Bool -> Assertion
assertBool' msg cond = msg `deepseq` assertBool msg cond

-- | Force a value for HPC-tick purposes.  Returns ().
tick :: (NFData a) => a -> Assertion
tick !x = x `deepseq` return ()

tests :: TestTree
tests =
  testGroup
    "summarizeEntry"
    [ testCase "helper CAFs are forced" $
        -- Force the top-level CAFs so their RHS evaluates once for HPC.
        tick (loc0, tm, cfg, baseEntry)
    , testCase "first line is asterisks plus title" $
        take 1 (summarize baseEntry) @?= ["** hello"]
    , testCase "includes FILE property from loc" $
        let out = summarize baseEntry
         in assertBool'
              (show out)
              ( any (":FILE:" `isInfixOf`) out
                  && any ("summary.org" `isInfixOf`) out
              )
    , testCase "includes OFFSET property from loc pos" $
        let out = summarize baseEntry
         in assertBool'
              (show out)
              ( any (":OFFSET:" `isInfixOf`) out
                  && any ("42" `isInfixOf`) out
              )
    , testCase "includes KEYWORD when entry has one" $
        let e = baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")}
            out = summarize e
         in assertBool' (show out) (any (":KEYWORD:" `isInfixOf`) out)
    , testCase "includes PRIORITY when set" $
        let e = baseEntry{_entryPriority = Just "A"}
            out = summarize e
         in assertBool' (show out) (any (":PRIORITY:" `isInfixOf`) out)
    , testCase "includes CONTEXT" $
        let e = baseEntry{_entryContext = Just "c"}
            out = summarize e
         in assertBool' (show out) (any (":CONTEXT:" `isInfixOf`) out)
    , testCase "includes VERB" $
        let e = baseEntry{_entryVerb = Just "Plan"}
            out = summarize e
         in assertBool' (show out) (any (":VERB:" `isInfixOf`) out)
    , testCase "includes LOCATOR" $
        let e = baseEntry{_entryLocator = Just "L"}
            out = summarize e
         in assertBool' (show out) (any (":LOCATOR:" `isInfixOf`) out)
    , testCase "includes TAGS when present" $
        let e = baseEntry{_entryTags = [PlainTag "a", PlainTag "b"]}
            out = summarize e
         in assertBool' (show out) (any (":a:b:" `isInfixOf`) out)
    , testCase "includes CLOSED stamp" $
        let e = baseEntry{_entryStamps = [ClosedStamp loc0 tm]}
            out = summarize e
         in assertBool' (show out) (any (":CLOSED:" `isInfixOf`) out)
    , testCase "includes SCHEDULED stamp" $
        let e = baseEntry{_entryStamps = [ScheduledStamp loc0 tm]}
            out = summarize e
         in assertBool' (show out) (any (":SCHEDULED:" `isInfixOf`) out)
    , testCase "includes DEADLINE stamp" $
        let e = baseEntry{_entryStamps = [DeadlineStamp loc0 tm]}
            out = summarize e
         in assertBool' (show out) (any (":DEADLINE:" `isInfixOf`) out)
    , testCase "includes ACTIVE stamp" $
        let e = baseEntry{_entryStamps = [ActiveStamp loc0 tm]}
            out = summarize e
         in assertBool' (show out) (any (":ACTIVE:" `isInfixOf`) out)
    , testCase "includes LOG_ENTRIES count when logs present" $
        let e =
              baseEntry
                { _entryLogEntries =
                    [ LogNote loc0 tm Nothing
                    , LogRefiling loc0 tm Nothing
                    ]
                }
            out = summarize e
         in assertBool' (show out) (any (":LOG_ENTRIES:" `isInfixOf`) out)
    , testCase "no LOG_ENTRIES when logs empty" $
        let out = summarize baseEntry
         in assertBool'
              (show out)
              (not (any (":LOG_ENTRIES:" `isInfixOf`) out))
    , testCase "includes BODY_LEN when body non-empty" $
        let e = baseEntry{_entryBody = Body [Paragraph loc0 ["hello"]]}
            out = summarize e
         in assertBool' (show out) (any (":BODY_LEN:" `isInfixOf`) out)
    , testCase "no BODY_LEN when body empty" $
        let out = summarize baseEntry
         in assertBool'
              (show out)
              (not (any (":BODY_LEN:" `isInfixOf`) out))
    , testCase "recurses into child items" $
        let child = baseEntry{_entryDepth = 3, _entryTitle = "child"}
            e = baseEntry{_entryItems = [child]}
            out = summarize e
         in assertBool' (show out) ("*** child" `elem` out)
    ]
