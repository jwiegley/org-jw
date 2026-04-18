module SluggifyTest (tests) where

import Org.Data (sluggify)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "sluggify"
    [ testCase "basic words" $
        sluggify "Hello World" @?= "hello-world"
    , testCase "strips ASCII apostrophes" $
        sluggify "what's up?" @?= "whats-up"
    , testCase "strips acute accents" $
        sluggify "Mar\237a" @?= "maria"
    , testCase "strips curly double quotes" $
        sluggify "\8220foo\8221" @?= "foo"
    , testCase "strips curly single quotes" $
        sluggify "\8216foo\8217" @?= "foo"
    , testCase "strips backticks" $
        sluggify "`foo`" @?= "foo"
    , testCase "collapses repeated underscores" $
        sluggify "a__b__c" @?= "a-b-c"
    , testCase "squashes non-alphanumerics" $
        sluggify "hello@world!" @?= "hello-world"
    , testCase "trims leading/trailing whitespace" $
        sluggify "   hi   " @?= "hi"
    , testCase "rewrites accented vowels" $
        sluggify "\225\237\250" @?= "aiu"
    ]
