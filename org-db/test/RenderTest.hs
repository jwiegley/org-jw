{-# LANGUAGE OverloadedStrings #-}

module RenderTest (tests) where

import qualified Data.Text as T
import Org.DB.Render
import Org.DB.Types
import Test.Tasty
import Test.Tasty.HUnit

sampleRow :: EntryRow
sampleRow =
  EntryRow
    { erId = "abc-123"
    , erFileId = "file-1"
    , erParentId = Nothing
    , erDepth = 2
    , erPosition = 0
    , erByteOffset = 42
    , erKeywordType = Just "open"
    , erKeywordValue = Just "TODO"
    , erPriority = Nothing
    , erHeadline = "TODO A \"quoted\" title"
    , erTitle = "A \"quoted\" title"
    , erVerb = Nothing
    , erContext = Nothing
    , erLocator = Nothing
    , erHash = Nothing
    , erModTime = Nothing
    , erCreatedTime = Nothing
    , erPath = Nothing
    }

tests :: TestTree
tests =
  testGroup
    "Org.DB.Render"
    [ testCase "emits file field when path known" $
        assertBool "file present" ("\"file\":\"todo.org\"" `T.isInfixOf` entryRowJson (Just "todo.org") sampleRow)
    , testCase "omits file field when path unknown" $
        assertBool "no file field" (not ("\"file\"" `T.isInfixOf` entryRowJson Nothing sampleRow))
    , testCase "keeps existing fields" $
        assertBool "id+depth" (("\"id\":\"abc-123\"" `T.isInfixOf` out) && ("\"depth\":2" `T.isInfixOf` out))
    , testCase "escapes quotes in title" $
        assertBool "escaped" ("\\\"quoted\\\"" `T.isInfixOf` out)
    , testCase "escapes control characters in title as \\uXXXX" $ do
        let ctl = entryRowJson Nothing sampleRow{erTitle = "a\SOHb"}
        assertBool "\\u0001 present" ("\\u0001" `T.isInfixOf` ctl)
        assertBool "raw control byte absent" (not ("\SOH" `T.isInfixOf` ctl))
    , testCase "keyword is bare null when absent, quoted when present" $ do
        let noKw = entryRowJson Nothing sampleRow{erKeywordValue = Nothing}
        assertBool "bare null" ("\"keyword\":null" `T.isInfixOf` noKw)
        assertBool "quoted value" ("\"keyword\":\"TODO\"" `T.isInfixOf` out)
    ]
 where
  out = entryRowJson Nothing sampleRow
