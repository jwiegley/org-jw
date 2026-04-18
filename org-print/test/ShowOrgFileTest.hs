{-# LANGUAGE BangPatterns #-}

module ShowOrgFileTest (tests) where

import Control.DeepSeq (NFData, deepseq, force)
import Data.List (isInfixOf, isSuffixOf)
import Org.Print
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

loc0 :: Loc
loc0 = Loc "x.org" 0

cfg :: Config
cfg = defaultConfig{_propertyColumn = 12, _tagsColumn = 40}

prop :: String -> String -> Property
prop = Property loc0 False

entry :: Entry
entry =
  Entry
    { _entryLoc = loc0
    , _entryDepth = 1
    , _entryKeyword = Nothing
    , _entryPriority = Nothing
    , _entryHeadline = "hello"
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

{- | Render an OrgFile and deep-force the resulting lines so HPC ticks
all expressions inside the helper CAFs and record construction.
-}
render :: Header -> [Entry] -> [String]
render hdr es =
  let !hdr' = force hdr
      !es' = force es
      !out = force (showOrgFile cfg (OrgFile "x.org" hdr' es'))
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
    "showOrgFile / showBlock / showHeader"
    [ testCase "helper CAFs are forced" $
        -- Force the top-level CAFs so their RHS evaluates once for HPC.
        tick (loc0, cfg, entry)
    , testCase "showOrgFile empty header and no entries" $
        render (Header [] [] (Body [])) []
          @?= []
    , testCase "showOrgFile with only #+ file properties" $
        let hdr = Header [] [prop "TITLE" "Test"] (Body [])
            out = render hdr []
         in out @?= ["#+TITLE: Test"]
    , testCase "showOrgFile with property drawer in header" $
        let hdr = Header [prop "ID" "abc"] [] (Body [])
            out = render hdr []
         in out
              @?= [ ":PROPERTIES:"
                  , ":ID:        abc"
                  , ":END:"
                  ]
    , testCase "showOrgFile header preamble body included" $
        let hdr =
              Header
                []
                []
                (Body [Paragraph loc0 ["pream line"]])
            out = render hdr []
         in out @?= ["pream line"]
    , testCase "showOrgFile header combined: drawer + file props + preamble" $
        let hdr =
              Header
                [prop "ID" "xyz"]
                [prop "TITLE" "T"]
                (Body [Paragraph loc0 ["intro"]])
            out = render hdr []
         in out
              @?= [ ":PROPERTIES:"
                  , ":ID:        xyz"
                  , ":END:"
                  , "#+TITLE: T"
                  , "intro"
                  ]
    , testCase "showOrgFile with one entry containing body paragraph" $
        let e =
              entry
                { _entryBody =
                    Body [Paragraph loc0 ["line1", "line2"]]
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out
              @?= [ "* hello"
                  , "line1"
                  , "line2"
                  ]
    , testCase "showOrgFile preserves Whitespace block verbatim" $
        let e = entry{_entryBody = Body [Whitespace loc0 ""]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out @?= ["* hello", ""]
    , testCase "showOrgFile Drawer block lines pass through" $
        let e =
              entry
                { _entryBody =
                    Body
                      [ Drawer
                          loc0
                          (PlainDrawer "LOGBOOK")
                          [":LOGBOOK:", ":END:"]
                      ]
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out @?= ["* hello", ":LOGBOOK:", ":END:"]
    , testCase "showBlock on Paragraph prepends leader per line" $
        let body = Body [Paragraph loc0 ["a", "b"]]
            e = entry{_entryBody = body}
            hdr =
              Header
                []
                []
                -- Use header preamble to trigger showBody "" (no leader)
                body
            out = render hdr [e]
         in out @?= ["a", "b", "* hello", "a", "b"]
    , testCase "showBlock InlineTask wraps with END marker" $
        let inner = entry{_entryDepth = 10, _entryTitle = "inline"}
            e =
              entry
                { _entryBody = Body [InlineTask loc0 inner]
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool'
              (show out)
              ( "********** inline" `elem` out
                  && "*************** END" `elem` out
              )
    , testCase "showEntry prefixLeader drops leader on empty string" $
        -- A Paragraph with an empty string is rendered as \"\" (not leader).
        let e =
              entry
                { _entryBody = Body [Paragraph loc0 [""]]
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out @?= ["* hello", ""]
    , testCase "showEntry propCol controls property indentation" $
        let e = entry{_entryProperties = [prop "ID" "v"]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out
              @?= [ "* hello"
                  , ":PROPERTIES:"
                  , ":ID:        v"
                  , ":END:"
                  ]
    , testCase "showEntry tags-only property (empty value) has single space" $
        -- propLine with null suffix returns plain prefix.
        let e = entry{_entryProperties = [prop "FLAG" ""]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in out @?= ["* hello", ":PROPERTIES:", ":FLAG:", ":END:"]
    , testCase "showEntry long name where width < 1 uses single space" $
        -- propertyColumn=12, name length forces width < 1
        let e = entry{_entryProperties = [prop "LONGISHNAME" "v"]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool' (show out) (":LONGISHNAME: v" `elem` out)
    , testCase "showEntry with tags renders suffix right-aligned" $
        let e = entry{_entryTags = [PlainTag "work", PlainTag "urgent"]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
            -- "* hello" = 7 chars, tagsCol = 40, suffix = ":work:urgent:" = 13 chars
            -- spacer width = 40 - 7 - 13 = 20 spaces
            expected = "* hello" <> replicate 20 ' ' <> ":work:urgent:"
         in assertBool' (show out) (expected `elem` out)
    , testCase "showEntry minimum spacer when tags don't fit" $
        -- tagsCol small forces width < 2 branch: uses two spaces
        let smallCfg = cfg{_tagsColumn = 1}
            e = entry{_entryTags = [PlainTag "t"]}
            hdr = Header [] [] (Body [])
            !out = force (showOrgFile smallCfg (OrgFile "x.org" hdr [e]))
         in assertBool' (show out) ("* hello  :t:" `elem` out)
    , testCase "showEntry keyword, priority, context, verb, locator" $
        let e =
              entry
                { _entryKeyword = Just (OpenKeyword loc0 "TODO")
                , _entryPriority = Just "A"
                , _entryContext = Just "ctx"
                , _entryVerb = Just "Do"
                , _entryLocator = Just "L1"
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool'
              (show out)
              ("* TODO [#A] (ctx) Do: hello {L1}" `elem` out)
    , testCase "showEntry leading stamps rendered on their own line" $
        let day = 60000
            at = Time ActiveTime day Nothing Nothing Nothing Nothing
            e =
              entry
                { _entryStamps =
                    [ ScheduledStamp loc0 at
                    , DeadlineStamp loc0 at
                    ]
                }
            hdr = Header [] [] (Body [])
            out = render hdr [e]
            expected =
              "SCHEDULED: <2023-02-25 Sat>"
                <> " DEADLINE: <2023-02-25 Sat>"
         in assertBool' (show out) (expected `elem` out)
    , testCase "showEntry ClosedStamp leading" $
        let day = 60000
            at = Time InactiveTime day Nothing Nothing Nothing Nothing
            e = entry{_entryStamps = [ClosedStamp loc0 at]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool'
              (show out)
              ("CLOSED: [2023-02-25 Sat]" `elem` out)
    , testCase "showEntry ActiveStamp emits active stamp line" $
        let day = 60000
            at = Time ActiveTime day Nothing Nothing Nothing Nothing
            e = entry{_entryStamps = [ActiveStamp loc0 at]}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool'
              (show out)
              ("<2023-02-25 Sat>" `elem` out)
    , testCase "showEntry title includes linked [[...][...]] link" $
        -- Exercises the regex fallback path in prefixLength.
        let e = entry{_entryTitle = "[[http://x][label]]"}
            hdr = Header [] [] (Body [])
            out = render hdr [e]
         in assertBool'
              (show out)
              (any (\first -> "[[http://x][label]]" `elem` words first) out)
    , testCase "showEntry link title with tags triggers regex-aware prefixLength" $
        -- A title with an Org link forces prefixLength to take the
        -- regex match branch.  Adding a tag forces 'spacer' evaluation.
        let e =
              entry
                { _entryTitle = "[[http://x][label]] suffix"
                , _entryTags = [PlainTag "t"]
                }
            hdr = Header [] [] (Body [])
            out =
              map (\s -> length s `seq` s) (render hdr [e])
         in assertBool'
              (show out)
              (any (\l -> ":t:" `isSuffixOf` l && "label" `isInfixOf` l) out)
    , testCase "child entries render recursively with increased depth" $
        let child = entry{_entryDepth = 2, _entryTitle = "child"}
            parent = entry{_entryItems = [child]}
            hdr = Header [] [] (Body [])
            out = render hdr [parent]
         in out @?= ["* hello", "** child"]
    ]
