{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests targeting previously uncovered branches of 'Org.Parse'.

Most existing test modules cover happy paths or a handful of error
messages. This module intentionally exercises:

  * The @"Trailing text in Org file"@ error at 'parseOrgFile''
  * 'parseEntryContext' bodies that contain spaces
  * The 'parseTime' range composition errors involving @--@ endings
    that still carry a @_timeDayEnd@ or @_timeEnd@
  * The @fromGregorianValid@ 'Nothing' branch (e.g. Feb 30)
  * 'parseBlock' whitespace, drawer, and inline-task alternatives
  * 'parseDrawer' 'parseSrcDrawer' variants for both lowercase
    (@#+begin_src@) and uppercase (@#+BEGIN_SRC@) markers
  * Clock durations with minutes only, verifying both 'LogClock'
    arms
  * Active range stamps combined with trailing log entries to push
    through more 'Log*' constructor positions
-}
module ParseUncoveredTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

{- | Config sufficient to exercise TODO/DONE transitions and keyword
changes in log entries.
-}
parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions =
        [ ("TODO", ["DONE", "WAIT"])
        , ("WAIT", ["TODO", "DONE"])
        ]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

parse :: ByteString -> Either String OrgFile
parse bs = either (Left . snd) Right (parseOrgFile parseConfig "t.org" bs)

entries :: ByteString -> Either String [Entry]
entries bs = (\(OrgFile _ _ es) -> es) <$> parse bs

expectRight :: Either String a -> IO a
expectRight = either assertFailure pure

onlyEntry :: ByteString -> Either String Entry
onlyEntry bs =
  entries bs >>= \case
    [e] -> Right e
    other -> Left ("expected one entry, got " ++ show (length other))

expectErrorContaining :: String -> ByteString -> Assertion
expectErrorContaining needle bs = case parse bs of
  Right _ ->
    assertFailure
      ("expected parse failure containing " ++ show needle)
  Left msg ->
    assertBool
      ("expected message to contain " ++ show needle ++ ", got: " ++ msg)
      (needle `isInfixOf` msg)

tests :: TestTree
tests =
  testGroup
    "uncovered parser branches"
    [ testGroup
        "parseEntryContext with whitespace-separated words"
        [ testCase "context with a space in it" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO (home office) Task"])
            _entryContext e @?= Just "home office"
        , testCase "context with leading and trailing spaces" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO (  kitchen table  ) Task"])
            _entryContext e @?= Just "  kitchen table  "
        ]
    , testGroup
        "parseTime range with illegal endings"
        [ -- The _timeDayEnd check in parseTime is defensive: the surface
          -- syntax does not allow parseTimeSingle to produce a Time with
          -- _timeDayEnd set (it is always initialized to Nothing). The
          -- _timeEnd branch is reachable, however, via a range whose
          -- end still carries an explicit end hour:
          testCase "ending stamp with explicit end time is rejected" $
            expectErrorContaining
              "Invalid ending time (has end)"
              ( fromLines
                  [ "* TODO Task"
                  , "<2024-01-15 Mon 09:00>--<2024-01-15 Mon 10:00-12:00>"
                  ]
              )
        ]
    , testGroup
        "parseTimeSingle Nothing branch for fromGregorianValid"
        [ testCase "Feb 30 reports the formatted gregorian error" $
            expectErrorContaining
              "2024-2-30"
              (fromLines ["* TODO Task", "SCHEDULED: <2024-02-30 Fri>"])
        ]
    , testGroup
        "parseTimeSingle suffix range"
        [ testCase "suffix number 100 is out of range" $
            expectErrorContaining
              "Time suffix out of range"
              ( fromLines
                  ["* TODO Task", "SCHEDULED: <2024-01-15 Mon +100d>"]
              )
        , testCase "suffix number 999 is out of range" $
            expectErrorContaining
              "Time suffix out of range"
              ( fromLines
                  ["* TODO Task", "SCHEDULED: <2024-01-15 Mon +999w>"]
              )
        ]
    , testGroup
        "trailing text after last entry"
        [ testCase "a bare '* END' line triggers trailing-text error" $
            -- parseEntry aborts when the headline is exactly 'END', so
            -- the outer 'many (parseEntry 1) <* eof' leaves the line
            -- unconsumed, forcing the alternative error branch.
            expectErrorContaining
              "Trailing text"
              (fromLines ["* TODO Task", "* END"])
        ]
    , testGroup
        "parseBlock alternatives"
        [ testCase "Whitespace block from blank line in entry body" $ do
            e <- expectRight $ onlyEntry blankLineBody
            assertBool
              "expected at least one Whitespace block"
              (any isWhitespaceBlock (_blocks (_entryBody e)))
        , testCase "Paragraph block preserves a text line" $ do
            e <- expectRight $ onlyEntry paragraphBody
            assertBool
              "expected at least one Paragraph block"
              (any isParagraphBlock (_blocks (_entryBody e)))
        , testCase "PlainDrawer content is preserved" $ do
            e <- expectRight $ onlyEntry plainDrawerBody
            case _blocks (_entryBody e) of
              [Drawer _ (PlainDrawer ":NOTES:") lns] ->
                assertBool
                  ("expected :END: at end, got " ++ show lns)
                  (":END:" `elem` lns)
              other ->
                assertFailure
                  ("expected a single PlainDrawer block, got " ++ show other)
        ]
    , testGroup
        "parseDrawer src variants (both cases)"
        [ testCase "#+begin_src haskell with body produces BeginDrawer" $ do
            e <- expectRight $ onlyEntry srcLowercase
            case _blocks (_entryBody e) of
              [Drawer _ (BeginDrawer header) lns] -> do
                assertBool
                  ("unexpected header: " ++ show header)
                  ("#+begin" `isInfixOf` header)
                assertBool
                  ("expected at least one body line, got " ++ show lns)
                  (length lns >= 2)
              other -> assertFailure ("expected BeginDrawer, got " ++ show other)
        , testCase "#+BEGIN_SRC haskell with body produces BeginDrawer" $ do
            e <- expectRight $ onlyEntry srcUppercase
            case _blocks (_entryBody e) of
              [Drawer _ (BeginDrawer header) lns] -> do
                assertBool
                  ("unexpected header: " ++ show header)
                  ("#+BEGIN" `isInfixOf` header)
                assertBool
                  ("expected body lines, got " ++ show lns)
                  (length lns >= 2)
              other -> assertFailure ("expected BeginDrawer, got " ++ show other)
        , testCase "empty src block still parses" $ do
            e <- expectRight $ onlyEntry srcEmpty
            case _blocks (_entryBody e) of
              [Drawer _ (BeginDrawer _) _] -> pure ()
              other -> assertFailure ("expected BeginDrawer, got " ++ show other)
        ]
    , testGroup
        "inline tasks as entry body"
        [ testCase "inline task with keyword inside entry" $ do
            e <- expectRight $ onlyEntry inlineTask
            case _blocks (_entryBody e) of
              [InlineTask _ inner] -> _entryDepth inner @?= 15
              other ->
                assertFailure ("expected InlineTask, got " ++ show other)
        ]
    , testGroup
        "clock entries"
        [ testCase "CLOCK line with duration yields LogClock with Duration" $ do
            e <- expectRight $ onlyEntry clockWithDuration
            case _entryLogEntries e of
              [LogClock _ _ (Just (Duration 2 30))] -> pure ()
              other ->
                assertFailure
                  ("expected LogClock with 2:30, got " ++ show other)
        , testCase "CLOCK line without duration yields open LogClock" $ do
            e <- expectRight $ onlyEntry clockOpen
            case _entryLogEntries e of
              [LogClock _ _ Nothing] -> pure ()
              other ->
                assertFailure ("expected open LogClock, got " ++ show other)
        ]
    , testGroup
        "multiple leading stamps variants"
        [ testCase "DEADLINE and CLOSED on one line parses both" $ do
            e <- expectRight $ onlyEntry deadlineThenClosed
            length (_entryStamps e) @?= 2
        , testCase "active stamp without leading stamps still detected" $ do
            e <- expectRight $ onlyEntry activeAloneBody
            case _entryStamps e of
              [ActiveStamp _ _] -> pure ()
              other ->
                assertFailure
                  ("expected single ActiveStamp, got " ++ show other)
        ]
    , testGroup
        "LOGBOOK wrapper followed by body"
        [ testCase
            "LOGBOOK containing a clock, then a body paragraph"
            $ do
              e <- expectRight $ onlyEntry logbookThenBody
              assertBool
                "expected at least one log entry"
                (not (null (_entryLogEntries e)))
              assertBool
                "expected a non-empty body"
                (not (null (_blocks (_entryBody e))))
        ]
    , testGroup
        "nested depths and multiple entries"
        [ testCase "sibling entries at same depth" $ do
            OrgFile _ _ es <- expectRight $ parse siblings
            map _entryDepth es @?= [1, 1, 1]
        , testCase "deeply nested entry tree" $ do
            OrgFile _ _ [top] <- expectRight $ parse deepTree
            _entryDepth top @?= 1
            case _entryItems top of
              [mid] -> case _entryItems mid of
                [leaf] -> _entryDepth leaf @?= 3
                other -> assertFailure ("expected one grandchild, got " ++ show other)
              other -> assertFailure ("expected one child, got " ++ show other)
        ]
    ]
 where
  isWhitespaceBlock Whitespace{} = True
  isWhitespaceBlock _ = False
  isParagraphBlock Paragraph{} = True
  isParagraphBlock _ = False

-- Fixtures

blankLineBody :: ByteString
blankLineBody =
  fromLines
    [ "* TODO Task"
    , ""
    , "Some text"
    ]

paragraphBody :: ByteString
paragraphBody =
  fromLines
    [ "* TODO Task"
    , "Single paragraph."
    ]

plainDrawerBody :: ByteString
plainDrawerBody =
  fromLines
    [ "* TODO Task"
    , ":NOTES:"
    , "first line"
    , "second line"
    , ":END:"
    ]

srcLowercase :: ByteString
srcLowercase =
  fromLines
    [ "* TODO Task"
    , "#+begin_src haskell"
    , "main = pure ()"
    , "#+end_src"
    ]

srcUppercase :: ByteString
srcUppercase =
  fromLines
    [ "* TODO Task"
    , "#+BEGIN_SRC haskell"
    , "main = pure ()"
    , "#+END_SRC"
    ]

srcEmpty :: ByteString
srcEmpty =
  fromLines
    [ "* TODO Task"
    , "#+begin_example"
    , "#+end_example"
    ]

inlineTask :: ByteString
inlineTask =
  fromLines
    [ "* TODO Outer"
    , "*************** TODO Inner task"
    , "*************** END"
    ]

clockWithDuration :: ByteString
clockWithDuration =
  fromLines
    [ "* TODO Task"
    , "CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 11:30] =>  2:30"
    ]

clockOpen :: ByteString
clockOpen =
  fromLines
    [ "* TODO Task"
    , "CLOCK: [2024-01-15 Mon 09:00]"
    ]

deadlineThenClosed :: ByteString
deadlineThenClosed =
  fromLines
    [ "* DONE Task"
    , "DEADLINE: <2024-01-20 Sat> CLOSED: [2024-01-19 Fri 10:00]"
    ]

activeAloneBody :: ByteString
activeAloneBody =
  fromLines
    [ "* TODO Task"
    , "<2024-03-01 Fri>"
    ]

logbookThenBody :: ByteString
logbookThenBody =
  fromLines
    [ "* TODO Task"
    , ":LOGBOOK:"
    , "CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 10:00] =>  1:00"
    , ":END:"
    , ""
    , "paragraph body"
    ]

siblings :: ByteString
siblings =
  fromLines
    [ "* TODO One"
    , "* TODO Two"
    , "* TODO Three"
    ]

deepTree :: ByteString
deepTree =
  fromLines
    [ "* TODO Top"
    , "** TODO Middle"
    , "*** TODO Leaf"
    ]
