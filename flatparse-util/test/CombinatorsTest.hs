{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CombinatorsTest (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad.Except (runExcept)
import Data.ByteString (ByteString)
import FlatParse.Combinators
import FlatParse.Stateful (Result (..), empty, eof, err, runParser, some, string)
import qualified FlatParse.Stateful as FP
import Test.Tasty
import Test.Tasty.HUnit

-- Parsing helpers.

run :: FP.Parser () String a -> ByteString -> Result String a
run p = runParser p () 0

okValue :: Result e a -> Maybe a
okValue (OK a _ _) = Just a
okValue _ = Nothing

-- A parser that always fails with an error message.
errA :: FP.Parser () String a
errA = err "boom"

-- A parser that always silently fails (Fail, not Err).
failA :: FP.Parser () String a
failA = empty

-- liftResult / resultToEither / parseMaybe

liftResultTests :: TestTree
liftResultTests =
  testGroup
    "liftResult"
    [ testCase "OK returns value" $
        runExcept (liftResult "f" (OK (42 :: Int) 0 "" :: Result String Int))
          @?= Right 42
    , testCase "Err propagates error" $
        runExcept (liftResult "f" (Err "bad" :: Result String Int))
          @?= Left "bad"
    , testCase "Fail raises error call" $ do
        r <-
          try (evaluate (runExcept (liftResult "f" (Fail :: Result String Int)))) ::
            IO (Either ErrorCall (Either String Int))
        case r of
          Left _ -> pure ()
          Right _ -> assertFailure "expected error call for Fail"
    ]

resultToEitherTests :: TestTree
resultToEitherTests =
  testGroup
    "resultToEither"
    [ testCase "OK becomes Right" $
        resultToEither "f" (OK (1 :: Int) 0 "" :: Result String Int)
          @?= Right 1
    , testCase "Err becomes Left" $
        resultToEither "f" (Err "nope" :: Result String Int) @?= Left "nope"
    , testCase "Fail raises error call" $ do
        r <-
          try (evaluate (resultToEither "f" (Fail :: Result String Int))) ::
            IO (Either ErrorCall (Either String Int))
        case r of
          Left _ -> pure ()
          Right _ -> assertFailure "expected error call for Fail"
    ]

parseMaybeTests :: TestTree
parseMaybeTests =
  testGroup
    "parseMaybe"
    [ testCase "OK returns Just" $
        parseMaybe () ($(string "ab") :: FP.Parser () String ()) "ab" @?= Just ()
    , testCase "Err returns Nothing" $
        parseMaybe () (errA :: FP.Parser () String ()) "x" @?= Nothing
    , testCase "Fail returns Nothing" $
        parseMaybe () (failA :: FP.Parser () String ()) "x" @?= Nothing
    ]

-- count / between / sepBy1 / endBy1

countTests :: TestTree
countTests =
  testGroup
    "count"
    [ testCase "count 0 yields empty list" $
        okValue (run (count 0 digitChar) "123") @?= Just ""
    , testCase "count 3 consumes exactly 3" $
        okValue (run (count 3 digitChar) "12345") @?= Just "123"
    , testCase "count fails when too few" $
        case run (count 3 digitChar) "12" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    ]

betweenTests :: TestTree
betweenTests =
  testGroup
    "between"
    [ testCase "strips bracket parsers" $
        okValue
          ( run
              (between $(string "(") $(string ")") (some digitChar))
              "(42)"
          )
          @?= Just "42"
    ]

sepBy1Tests :: TestTree
sepBy1Tests =
  testGroup
    "sepBy1"
    [ testCase "single item" $
        okValue (run (sepBy1 digitChar $(string ",")) "5") @?= Just "5"
    , testCase "multiple items" $
        okValue (run (sepBy1 digitChar $(string ",")) "1,2,3") @?= Just "123"
    , testCase "fails on empty" $
        case run (sepBy1 digitChar $(string ",")) "" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    ]

endBy1Tests :: TestTree
endBy1Tests =
  testGroup
    "endBy1"
    [ testCase "one item with terminator" $
        okValue (run (endBy1 digitChar $(string ";")) "7;") @?= Just "7"
    , testCase "many items with terminators" $
        okValue (run (endBy1 digitChar $(string ";")) "1;2;3;") @?= Just "123"
    , testCase "fails without terminator" $
        case run (endBy1 digitChar $(string ";")) "1" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    ]

-- manyTill / manyTill_ / someTill

manyTillTests :: TestTree
manyTillTests =
  testGroup
    "manyTill"
    [ testCase "stops at terminator immediately" $
        okValue (run (manyTill digitChar $(string ".")) ".") @?= Just ""
    , testCase "collects characters until terminator" $
        okValue (run (manyTill digitChar $(string ".")) "123.") @?= Just "123"
    ]

manyTill_Tests :: TestTree
manyTill_Tests =
  testGroup
    "manyTill_"
    [ testCase "returns collected list and end token" $
        okValue (run (manyTill_ digitChar $(string "X")) "42X")
          @?= Just ("42", ())
    , testCase "empty list when terminator first" $
        okValue (run (manyTill_ digitChar $(string "X")) "X")
          @?= Just ("", ())
    ]

someTillTests :: TestTree
someTillTests =
  testGroup
    "someTill"
    [ testCase "requires at least one" $
        okValue (run (someTill digitChar $(string "!")) "3!") @?= Just "3"
    , testCase "collects multiple" $
        okValue (run (someTill digitChar $(string "!")) "123!") @?= Just "123"
    , testCase "fails on immediate terminator" $
        case run (someTill digitChar $(string "!")) "!" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    ]

-- Character / whitespace / line helpers

charHelperTests :: TestTree
charHelperTests =
  testGroup
    "character helpers"
    [ testCase "newline consumes '\\n'" $
        okValue (run (newline <* eof) "\n") @?= Just ()
    , testCase "singleSpace consumes ' '" $
        okValue (run (singleSpace <* eof) " ") @?= Just ()
    , testCase "spaces_ requires one or more" $
        okValue (run (spaces_ <* eof) "   ") @?= Just ()
    , testCase "spaces_ fails on empty" $
        case run spaces_ "" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    , testCase "digitChar reads a digit" $
        okValue (run digitChar "9") @?= Just '9'
    , testCase "trailingSpace consumes trailing spaces then newline" $
        okValue (run (trailingSpace <* eof) "   \n") @?= Just ()
    , testCase "trailingSpace with no spaces" $
        okValue (run (trailingSpace <* eof) "\n") @?= Just ()
    ]

lineHelperTests :: TestTree
lineHelperTests =
  testGroup
    "line helpers"
    [ testCase "lineOrEof takes a line" $
        okValue (run lineOrEof "hello\nrest") @?= Just "hello"
    , testCase "wholeLine delegates to takeLine" $
        okValue (run wholeLine "abc\n") @?= Just "abc"
    , testCase "restOfLine delegates to takeLine" $
        okValue (run restOfLine "xyz\n") @?= Just "xyz"
    ]

identifierTests :: TestTree
identifierTests =
  testGroup
    "identifier"
    [ testCase "alphanumeric" $
        okValue (run identifier "abc123") @?= Just "abc123"
    , testCase "allows underscore and space" $
        okValue (run identifier "a_b c") @?= Just "a_b c"
    , testCase "stops at punctuation" $
        okValue (run (identifier <* $(string "!")) "name!") @?= Just "name"
    , testCase "fails on empty" $
        case run identifier "" of
          OK _ _ _ -> assertFailure "expected failure"
          _ -> pure ()
    ]

tests :: TestTree
tests =
  testGroup
    "FlatParse.Combinators"
    [ liftResultTests
    , resultToEitherTests
    , parseMaybeTests
    , countTests
    , betweenTests
    , sepBy1Tests
    , endBy1Tests
    , manyTillTests
    , manyTill_Tests
    , someTillTests
    , charHelperTests
    , lineHelperTests
    , identifierTests
    ]
