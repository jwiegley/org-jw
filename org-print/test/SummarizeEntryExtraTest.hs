module SummarizeEntryExtraTest (tests) where

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

-- Force full string evaluation so HPC marks internal Property ticks covered.
forceLines :: [String] -> [String]
forceLines = map (\s -> length s `seq` s)

tests :: TestTree
tests =
  testGroup
    "summarizeEntry (forced)"
    [ testCase "FILE property value matches exactly" $
        let out = forceLines (summarizeEntry cfg baseEntry)
         in assertBool
              (show out)
              (":FILE: summary.org" `elem` out)
    , testCase "OFFSET property value matches exactly" $
        let out = forceLines (summarizeEntry cfg baseEntry)
         in assertBool
              (show out)
              (":OFFSET: 42" `elem` out)
    , testCase "KEYWORD property shows underlying show repr" $
        let e =
              baseEntry{_entryKeyword = Just (OpenKeyword loc0 "TODO")}
            out = forceLines (summarizeEntry cfg e)
         in -- show (OpenKeyword loc0 "TODO") includes the Loc
            assertBool
              (show out)
              (any (\ln -> ":KEYWORD:" `elem` words ln) out)
    , testCase "PRIORITY exact value" $
        let e = baseEntry{_entryPriority = Just "B"}
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":PRIORITY: B" `elem` out)
    , testCase "CONTEXT exact value" $
        let e = baseEntry{_entryContext = Just "home"}
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":CONTEXT: home" `elem` out)
    , testCase "VERB exact value" $
        let e = baseEntry{_entryVerb = Just "Review"}
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":VERB: Review" `elem` out)
    , testCase "LOCATOR exact value" $
        let e = baseEntry{_entryLocator = Just "L"}
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":LOCATOR: L" `elem` out)
    , testCase "TAGS joined with colons" $
        let e =
              baseEntry
                { _entryTags = [PlainTag "a", PlainTag "b", PlainTag "c"]
                }
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              (":TAGS: :a:b:c:" `elem` out)
    , testCase "LOG_ENTRIES count exactly" $
        let e =
              baseEntry
                { _entryLogEntries =
                    [ LogNote loc0 tm Nothing
                    , LogNote loc0 tm Nothing
                    , LogNote loc0 tm Nothing
                    ]
                }
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":LOG_ENTRIES: 3" `elem` out)
    , testCase "BODY_LEN exact count" $
        let e =
              baseEntry
                { _entryBody = Body [Paragraph loc0 ["abc"]]
                }
            out = forceLines (summarizeEntry cfg e)
         in assertBool (show out) (":BODY_LEN: 3" `elem` out)
    , testCase "CLOSED stamp exact" $
        let e = baseEntry{_entryStamps = [ClosedStamp loc0 tm]}
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              (":CLOSED: [2023-02-25 Sat]" `elem` out)
    , testCase "SCHEDULED stamp exact" $
        let e = baseEntry{_entryStamps = [ScheduledStamp loc0 tm]}
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              (":SCHEDULED: [2023-02-25 Sat]" `elem` out)
    , testCase "DEADLINE stamp exact" $
        let e = baseEntry{_entryStamps = [DeadlineStamp loc0 tm]}
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              (":DEADLINE: [2023-02-25 Sat]" `elem` out)
    , testCase "ACTIVE stamp exact" $
        let e = baseEntry{_entryStamps = [ActiveStamp loc0 tm]}
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              (":ACTIVE: [2023-02-25 Sat]" `elem` out)
    , testCase "child items recurse in summarizeEntry" $
        let child =
              baseEntry
                { _entryDepth = 3
                , _entryTitle = "kid"
                , _entryPriority = Just "A"
                }
            e = baseEntry{_entryItems = [child]}
            out = forceLines (summarizeEntry cfg e)
         in assertBool
              (show out)
              ( "*** kid" `elem` out
                  && ":PRIORITY: A" `elem` out
              )
    ]
