{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.Parse.Combinators where

import Control.Monad.Except
import Data.ByteString (ByteString)
import FlatParse.Stateful hiding (Parser)
import FlatParse.Stateful qualified as FP

liftResult :: (MonadError e m) => Result e a -> m a
liftResult (OK res _ _) = pure res
liftResult (Err e) = throwError e
liftResult Fail = error "The worst happened"

resultToEither :: Result e a -> Either e a
resultToEither (OK res _ _) = Right res
resultToEither (Err e) = Left e
resultToEither Fail = error "The worst happened"

parseMaybe :: FP.Parser () e a -> ByteString -> Maybe a
parseMaybe p s = case runParser p () 0 s of
  OK res _ _ -> Just res
  Err _ -> Nothing
  Fail -> Nothing

count ::
  Int ->
  FP.Parser r String a ->
  FP.Parser r String [a]
count cnt p = go cnt
  where
    go 0 = pure []
    go n = (:) <$> p <*> go (pred n)

between ::
  FP.Parser r String () ->
  FP.Parser r String () ->
  FP.Parser r String a ->
  FP.Parser r String a
between s e p = s *> p <* e

endBy1 ::
  FP.Parser r String a ->
  FP.Parser r String sep ->
  FP.Parser r String [a]
endBy1 p sep = some $ do
  x <- p
  _ <- sep
  return x

sepBy1 ::
  FP.Parser r String a ->
  FP.Parser r String sep ->
  FP.Parser r String [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

manyTill ::
  (Show a) =>
  FP.Parser r String a ->
  FP.Parser r String () ->
  FP.Parser r String [a]
manyTill p e = go []
  where
    go acc =
      (reverse acc <$ e)
        <|> (go . (: acc) =<< p)

manyTill_ ::
  FP.Parser r String a ->
  FP.Parser r String end ->
  FP.Parser r String ([a], end)
manyTill_ p e = go []
  where
    go !acc = do
      (let !y = reverse acc in ((y,) <$> e))
        <|> (go . (: acc) =<< p)

someTill ::
  FP.Parser r String a ->
  FP.Parser r String () ->
  FP.Parser r String [a]
someTill p e = go []
  where
    go acc = do
      x <- p
      ((x : acc) <$ e)
        <|> go (x : acc)

newline :: FP.Parser r String ()
newline = $(char '\n')

singleSpace :: FP.Parser r String ()
singleSpace = $(char ' ')

spaces_ :: FP.Parser r String ()
spaces_ = skipSome singleSpace

digitChar :: FP.Parser r String Char
digitChar = satisfy isDigit

trailingSpace :: FP.Parser r String ()
trailingSpace = skipMany singleSpace <* newline

lineOrEof :: FP.Parser r String String
lineOrEof = takeLine

wholeLine :: FP.Parser r String String
wholeLine = takeLine

restOfLine :: FP.Parser r String String
restOfLine = takeLine

identifier :: FP.Parser r String String
identifier = some (satisfy (\c -> isAlphaNum c || c == '_' || c == ' '))
