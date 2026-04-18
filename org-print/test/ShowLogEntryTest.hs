module ShowLogEntryTest (tests) where

import Control.Monad.Reader (runReader)
import Data.List (isInfixOf, isPrefixOf)
import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "t.org" 0

tm :: Time
tm = Time InactiveTime 60000 Nothing Nothing Nothing Nothing

tm2 :: Time
tm2 = tm{_timeDay = 60001}

cfg :: Config
cfg = defaultConfig{_tagsColumn = 60, _propertyColumn = 11}

entryWithLog :: LogEntry -> Entry
entryWithLog le =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "x"
    , _entryVerb = Nothing
    , _entryTitle = "x"
    , _entryContext = Nothing
    , _entryLocator = Nothing
    , _entryTags = []
    , _entryStamps = []
    , _entryProperties = []
    , _entryLogEntries = [le]
    , _entryBody = Body []
    , _entryItems = []
    }

render :: LogEntry -> [String]
render le = runReader (showEntry (entryWithLog le)) cfg

tests :: TestTree
tests =
  testGroup
    "showLogEntry arms"
    [ testCase "LogClosing without body" $
        let out = render (LogClosing loc0 tm Nothing)
         in assertBool (show out) (any ("- CLOSING NOTE" `isInfixOf`) out)
    , testCase "LogClosing with body adds line-break marker" $
        let out =
              render
                (LogClosing loc0 tm (Just (Body [Paragraph loc0 ["note"]])))
         in assertBool (show out) (any (" \\\\" `isInfixOf`) out)
    , testCase "LogState with no previous keyword" $
        let out =
              render (LogState loc0 (OpenKeyword loc0 "DONE") Nothing tm Nothing)
         in assertBool (show out) (any ("- State" `isInfixOf`) out)
    , testCase "LogState with previous keyword" $
        let out =
              render
                ( LogState
                    loc0
                    (OpenKeyword loc0 "DONE")
                    (Just (OpenKeyword loc0 "TODO"))
                    tm
                    Nothing
                )
         in assertBool (show out) (any ("from \"TODO\"" `isInfixOf`) out)
    , testCase "LogNote" $
        let out = render (LogNote loc0 tm Nothing)
         in assertBool (show out) (any ("- Note taken on" `isInfixOf`) out)
    , testCase "LogRescheduled" $
        let out = render (LogRescheduled loc0 tm tm2 Nothing)
         in assertBool
              (show out)
              (any ("- Rescheduled from" `isInfixOf`) out)
    , testCase "LogNotScheduled" $
        let out = render (LogNotScheduled loc0 tm tm2 Nothing)
         in assertBool
              (show out)
              (any ("- Not scheduled, was" `isInfixOf`) out)
    , testCase "LogDeadline" $
        let out = render (LogDeadline loc0 tm tm2 Nothing)
         in assertBool
              (show out)
              (any ("- New deadline from" `isInfixOf`) out)
    , testCase "LogNoDeadline" $
        let out = render (LogNoDeadline loc0 tm tm2 Nothing)
         in assertBool
              (show out)
              (any ("- Removed deadline, was" `isInfixOf`) out)
    , testCase "LogRefiling" $
        let out = render (LogRefiling loc0 tm Nothing)
         in assertBool (show out) (any ("- Refiled on" `isInfixOf`) out)
    , testCase "LogClock without duration" $
        let out = render (LogClock loc0 tm Nothing)
         in assertBool (show out) (any ("CLOCK:" `isPrefixOf`) out)
    , testCase "LogClock duration single-digit hour pads with space" $
        let out = render (LogClock loc0 tm (Just (Duration 1 30)))
         in assertBool (show out) (any (" =>  1:30" `isInfixOf`) out)
    , testCase "LogClock duration zero-pads minutes" $
        let out = render (LogClock loc0 tm (Just (Duration 10 5)))
         in assertBool (show out) (any (" => 10:05" `isInfixOf`) out)
    , testCase "LogClock duration handles multi-digit hours" $
        let out = render (LogClock loc0 tm (Just (Duration 123 45)))
         in assertBool (show out) (any (" => 123:45" `isInfixOf`) out)
    , testCase "LogBook wraps in LOGBOOK markers" $
        let out = render (LogBook loc0 [LogNote loc0 tm Nothing])
         in assertBool
              (show out)
              ( any (":LOGBOOK:" `isInfixOf`) out
                  && any (":END:" `isInfixOf`) out
              )
    , testCase "LogNote with body adds trailing marker" $
        let out =
              render
                (LogNote loc0 tm (Just (Body [Paragraph loc0 ["n"]])))
         in assertBool (show out) (any (" \\\\" `isInfixOf`) out)
    , testCase "showEntry without keyword renders plain title prefix" $
        let e = entryWithLog (LogNote loc0 tm Nothing)
            out = runReader (showEntry e) cfg
         in case out of
              (x : _) -> x @?= "* x"
              [] -> assertFailure "showEntry returned no lines"
    ]
