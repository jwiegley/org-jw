{-# LANGUAGE OverloadedStrings #-}

module FilePropertiesTest (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Org.Parse (parseOrgFile)
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit

parseConfig :: Config
parseConfig =
  defaultConfig
    { _openKeywords = ["TODO"]
    , _closedKeywords = ["DONE"]
    }

fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate "\n" . (++ [""])

parse :: ByteString -> Either String OrgFile
parse bs = either (Left . snd) Right (parseOrgFile parseConfig "t.org" bs)

expectRight :: Either String a -> IO a
expectRight = either assertFailure pure

tests :: TestTree
tests =
  testGroup
    "file header parsing"
    [ testCase "top-level :PROPERTIES: drawer is captured" $ do
        OrgFile _ h _ <- expectRight $ parse fileWithProps
        length (_headerPropertiesDrawer h) @?= 2
    , testCase "no property drawer, only file properties" $ do
        OrgFile _ h _ <- expectRight $ parse fileJustTitle
        _headerPropertiesDrawer h @?= []
        length (_headerFileProperties h) @?= 1
    , testCase "#+title and #+filetags are both captured" $ do
        OrgFile _ h _ <- expectRight $ parse fileTitleAndTags
        let names = map _name (_headerFileProperties h)
        names @?= ["title", "filetags"]
    , testCase "file property values are not trimmed of trailing content" $ do
        OrgFile _ h _ <- expectRight $ parse fileWithValue
        case _headerFileProperties h of
          [p] -> _value p @?= "hello world"
          other -> assertFailure ("expected one property, got " ++ show other)
    , testCase "empty file produces empty header and no entries" $ do
        OrgFile _ h es <- expectRight $ parse ""
        _headerPropertiesDrawer h @?= []
        _headerFileProperties h @?= []
        es @?= []
    , testCase "file with only preamble body yields no entries" $ do
        OrgFile _ _ es <- expectRight $ parse preambleOnly
        es @?= []
    , testCase "property without leading :PROPERTIES: is file property" $ do
        -- No :PROPERTIES: block; just a single #+... line.
        OrgFile _ h _ <- expectRight $ parse singleFileProp
        _headerPropertiesDrawer h @?= []
        length (_headerFileProperties h) @?= 1
    , testCase "file with category recognized as file property" $ do
        OrgFile _ h _ <- expectRight $ parse withCategory
        let names = map _name (_headerFileProperties h)
        names @?= ["category"]
    ]

fileWithProps :: ByteString
fileWithProps =
  fromLines
    [ ":PROPERTIES:"
    , ":ID:       FILE-ID"
    , ":CREATED:  [2024-01-01 Mon 09:00]"
    , ":END:"
    , "#+title: t"
    ]

fileJustTitle :: ByteString
fileJustTitle =
  fromLines
    [ "#+title: sample"
    ]

fileTitleAndTags :: ByteString
fileTitleAndTags =
  fromLines
    [ "#+title: sample"
    , "#+filetags: :foo:bar:"
    ]

fileWithValue :: ByteString
fileWithValue =
  fromLines
    [ "#+title: hello world"
    ]

preambleOnly :: ByteString
preambleOnly =
  fromLines
    [ "#+title: t"
    , ""
    , "Some preamble text."
    ]

singleFileProp :: ByteString
singleFileProp =
  fromLines
    [ "#+category: things"
    ]

withCategory :: ByteString
withCategory =
  fromLines
    [ "#+category: Work"
    ]
