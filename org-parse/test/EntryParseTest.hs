{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EntryParseTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO", "WAIT"]
    , _closedKeywords = ["DONE", "CANCELED"]
    , _keywordTransitions = [("TODO", ["DONE", "WAIT"])]
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

tests :: TestTree
tests =
  testGroup
    "parseEntry"
    [ testGroup
        "priorities"
        [ testCase "[#A] priority" $ do
            e <- expectRight $ onlyEntry (fromLines ["* TODO [#A] Task"])
            _entryPriority e @?= Just "A"
        , testCase "[#B] priority" $ do
            e <- expectRight $ onlyEntry (fromLines ["* TODO [#B] Task"])
            _entryPriority e @?= Just "B"
        , testCase "[#C] priority" $ do
            e <- expectRight $ onlyEntry (fromLines ["* TODO [#C] Task"])
            _entryPriority e @?= Just "C"
        ]
    , testGroup
        "title decorations"
        [ testCase "verb before title" $ do
            e <- expectRight $ onlyEntry (fromLines ["* TODO write: Something"])
            _entryVerb e @?= Just "write"
            _entryTitle e @?= "Something"
        , testCase "context before title" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO (kitchen) Do something"])
            _entryContext e @?= Just "kitchen"
        , testCase "title with location suffix" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO Task  {loc}"])
            _entryLocator e @?= Just "loc"
        , testCase "title with tags" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO Task  :foo:bar:"])
            _entryTags e @?= [PlainTag "foo", PlainTag "bar"]
        , testCase "title with location and tags" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO Task  {loc}  :foo:"])
            _entryLocator e @?= Just "loc"
            _entryTags e @?= [PlainTag "foo"]
        , testCase "empty title suffix (no tags)" $ do
            e <-
              expectRight $
                onlyEntry (fromLines ["* TODO Plain"])
            _entryLocator e @?= Nothing
            _entryTags e @?= []
        , testCase "context then verb before title" $ do
            e <-
              expectRight $
                onlyEntry
                  (fromLines ["* TODO (home) write: Task  {here}  :x:y:"])
            _entryContext e @?= Just "home"
            _entryVerb e @?= Just "write"
            _entryTitle e @?= "Task"
            _entryLocator e @?= Just "here"
            _entryTags e @?= [PlainTag "x", PlainTag "y"]
        ]
    , testGroup
        "nested entries"
        [ testCase "child of a parent increments depth" $ do
            OrgFile _ _ [p] <- expectRight $ parse nested
            _entryDepth p @?= 1
            case _entryItems p of
              [c] -> _entryDepth c @?= 2
              other -> assertFailure ("expected one child, got " ++ show other)
        , testCase "sibling top-level entries both depth 1" $ do
            OrgFile _ _ es <- expectRight $ parse twoSiblings
            length es @?= 2
            map _entryDepth es @?= [1, 1]
        ]
    , testGroup
        "drawers in body"
        [ testCase "plain drawer stored as Drawer block" $ do
            e <- expectRight $ onlyEntry plainDrawerBody
            case _blocks (_entryBody e) of
              [Drawer _ (PlainDrawer ":NOTES:") _] -> pure ()
              other ->
                assertFailure ("expected single PlainDrawer, got " ++ show other)
        , testCase "lowercase begin block" $ do
            e <- expectRight $ onlyEntry srcBlockLowercase
            case _blocks (_entryBody e) of
              [Drawer _ (BeginDrawer header) _] ->
                assertBool
                  ("got BeginDrawer " ++ show header)
                  ("#+begin" `prefix` header)
              other -> assertFailure ("expected BeginDrawer, got " ++ show other)
        , testCase "uppercase BEGIN block" $ do
            e <- expectRight $ onlyEntry srcBlockUppercase
            case _blocks (_entryBody e) of
              [Drawer _ (BeginDrawer header) _] ->
                assertBool
                  ("got BeginDrawer " ++ show header)
                  ("#+BEGIN" `prefix` header)
              other -> assertFailure ("expected BeginDrawer, got " ++ show other)
        ]
    , testGroup
        "properties drawer"
        [ testCase "entry :PROPERTIES: drawer is parsed" $ do
            e <- expectRight $ onlyEntry entryWithProps
            length (_entryProperties e) @?= 2
            map _name (_entryProperties e) @?= ["ID", "CREATED"]
        ]
    , testGroup
        "keywords without priority"
        [ testCase "open keyword only" $ do
            e <- expectRight $ onlyEntry (fromLines ["* TODO Task"])
            case _entryKeyword e of
              Just (OpenKeyword _ "TODO") -> pure ()
              other -> assertFailure ("expected OpenKeyword TODO, got " ++ show other)
        , testCase "closed keyword only" $ do
            e <- expectRight $ onlyEntry (fromLines ["* DONE Task"])
            case _entryKeyword e of
              Just (ClosedKeyword _ "DONE") -> pure ()
              other -> assertFailure ("expected ClosedKeyword DONE, got " ++ show other)
        , testCase "no keyword at all" $ do
            e <- expectRight $ onlyEntry (fromLines ["* Just a plain heading"])
            _entryKeyword e @?= Nothing
        ]
    , testGroup
        "inline tasks"
        [ testCase "inline task parses inside entry body" $ do
            e <- expectRight $ onlyEntry inlineTaskFixture
            case _blocks (_entryBody e) of
              [InlineTask _ _] -> pure ()
              other -> assertFailure ("expected InlineTask block, got " ++ show other)
        ]
    ]
 where
  prefix p s = take (length p) s == p

nested :: ByteString
nested =
  fromLines
    [ "* TODO Parent"
    , "** TODO Child"
    ]

twoSiblings :: ByteString
twoSiblings =
  fromLines
    [ "* TODO First"
    , "* TODO Second"
    ]

plainDrawerBody :: ByteString
plainDrawerBody =
  fromLines
    [ "* TODO Task"
    , ":NOTES:"
    , "some content"
    , ":END:"
    ]

srcBlockLowercase :: ByteString
srcBlockLowercase =
  fromLines
    [ "* TODO Task"
    , "#+begin_src haskell"
    , "putStrLn \"hi\""
    , "#+end_src"
    ]

srcBlockUppercase :: ByteString
srcBlockUppercase =
  fromLines
    [ "* TODO Task"
    , "#+BEGIN_SRC haskell"
    , "putStrLn \"hi\""
    , "#+END_SRC"
    ]

entryWithProps :: ByteString
entryWithProps =
  fromLines
    [ "* TODO Task"
    , ":PROPERTIES:"
    , ":ID:       E-1"
    , ":CREATED:  [2024-01-01 Mon 09:00]"
    , ":END:"
    ]

inlineTaskFixture :: ByteString
inlineTaskFixture =
  fromLines
    [ "* TODO Outer"
    , "*************** TODO Inline task body"
    , "*************** END"
    ]
