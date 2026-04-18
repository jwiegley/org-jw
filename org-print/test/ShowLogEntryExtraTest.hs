module ShowLogEntryExtraTest (tests) where

import Control.Monad.Reader (runReader)
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

-- Force every string line so HPC ticks internal concat expressions.
forceLines :: [String] -> [String]
forceLines = map (\s -> length s `seq` s)

noteBody :: Body
noteBody = Body [Paragraph loc0 ["note line"]]

tests :: TestTree
tests =
  testGroup
    "showLogEntry (full equality)"
    [ testCase "LogClosing full line equality" $
        let out = forceLines (render (LogClosing loc0 tm Nothing))
         in assertBool
              (show out)
              ("- CLOSING NOTE[2023-02-25 Sat]" `elem` out)
    , testCase "LogClosing with body produces indented body line" $
        let out =
              forceLines
                (render (LogClosing loc0 tm (Just noteBody)))
         in assertBool
              (show out)
              ("  note line" `elem` out)
    , testCase "LogNote full line equality" $
        let out = forceLines (render (LogNote loc0 tm Nothing))
         in assertBool
              (show out)
              ("- Note taken on [2023-02-25 Sat]" `elem` out)
    , testCase "LogNote with body line-break marker" $
        let out =
              forceLines (render (LogNote loc0 tm (Just noteBody)))
         in assertBool
              (show out)
              ( "- Note taken on [2023-02-25 Sat] \\\\" `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogRescheduled full line equality" $
        let out = forceLines (render (LogRescheduled loc0 tm tm2 Nothing))
            expected =
              "- Rescheduled from \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogRescheduled with body line-break marker" $
        let out =
              forceLines
                (render (LogRescheduled loc0 tm tm2 (Just noteBody)))
            expected =
              "- Rescheduled from \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun] \\\\"
         in assertBool
              (show out)
              ( expected `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogNotScheduled full line equality" $
        let out = forceLines (render (LogNotScheduled loc0 tm tm2 Nothing))
            expected =
              "- Not scheduled, was \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogNotScheduled with body line-break marker" $
        let out =
              forceLines
                (render (LogNotScheduled loc0 tm tm2 (Just noteBody)))
            expected =
              "- Not scheduled, was \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun] \\\\"
         in assertBool
              (show out)
              ( expected `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogDeadline full line equality" $
        let out = forceLines (render (LogDeadline loc0 tm tm2 Nothing))
            expected =
              "- New deadline from \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogDeadline with body line-break marker" $
        let out =
              forceLines
                (render (LogDeadline loc0 tm tm2 (Just noteBody)))
            expected =
              "- New deadline from \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun] \\\\"
         in assertBool
              (show out)
              ( expected `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogNoDeadline full line equality" $
        let out = forceLines (render (LogNoDeadline loc0 tm tm2 Nothing))
            expected =
              "- Removed deadline, was \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogNoDeadline with body line-break marker" $
        let out =
              forceLines
                (render (LogNoDeadline loc0 tm tm2 (Just noteBody)))
            expected =
              "- Removed deadline, was \"[2023-02-25 Sat]\""
                <> " on [2023-02-26 Sun] \\\\"
         in assertBool
              (show out)
              ( expected `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogRefiling full line equality" $
        let out = forceLines (render (LogRefiling loc0 tm Nothing))
         in assertBool
              (show out)
              ("- Refiled on [2023-02-25 Sat]" `elem` out)
    , testCase "LogRefiling with body line-break marker" $
        let out =
              forceLines (render (LogRefiling loc0 tm (Just noteBody)))
         in assertBool
              (show out)
              ( "- Refiled on [2023-02-25 Sat] \\\\" `elem` out
                  && "  note line" `elem` out
              )
    , testCase "LogClock without duration full line equality" $
        let out = forceLines (render (LogClock loc0 tm Nothing))
         in assertBool
              (show out)
              ("CLOCK: [2023-02-25 Sat]" `elem` out)
    , testCase "LogState full line equality with previous keyword" $
        let out =
              forceLines
                ( render
                    ( LogState
                        loc0
                        (OpenKeyword loc0 "DONE")
                        (Just (OpenKeyword loc0 "TODO"))
                        tm
                        Nothing
                    )
                )
            -- padded 13 on "\"DONE\"" (6 chars) => 7 trailing spaces
            -- padded 18 on "from \"TODO\"" (11 chars) => 7 trailing spaces
            expected =
              "- State \"DONE\"       from \"TODO\"       [2023-02-25 Sat]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogState full line equality without previous keyword" $
        let out =
              forceLines
                ( render
                    ( LogState
                        loc0
                        (ClosedKeyword loc0 "CANCELLED")
                        Nothing
                        tm
                        Nothing
                    )
                )
            -- padded 13 on "\"CANCELLED\"" (11 chars) => 2 trailing spaces
            expected = "- State \"CANCELLED\"  [2023-02-25 Sat]"
         in assertBool (show out) (expected `elem` out)
    , testCase "LogState with body adds trailing marker" $
        let out =
              forceLines
                ( render
                    ( LogState
                        loc0
                        (OpenKeyword loc0 "DONE")
                        Nothing
                        tm
                        (Just noteBody)
                    )
                )
         in assertBool (show out) ("  note line" `elem` out)
    , testCase "LogBook nested contents fully rendered" $
        let out =
              forceLines
                ( render
                    ( LogBook
                        loc0
                        [ LogNote loc0 tm Nothing
                        , LogRefiling loc0 tm Nothing
                        ]
                    )
                )
         in assertBool
              (show out)
              ( ":LOGBOOK:" `elem` out
                  && "- Note taken on [2023-02-25 Sat]" `elem` out
                  && "- Refiled on [2023-02-25 Sat]" `elem` out
                  && ":END:" `elem` out
              )
    ]
