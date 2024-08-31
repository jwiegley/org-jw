{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FlatParse.Combinators where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import FlatParse.Stateful hiding (Parser)
import FlatParse.Stateful qualified as FP

liftResult :: (MonadError e m) => FilePath -> Result e a -> m a
liftResult _ (OK res _ _) = pure res
liftResult _ (Err e) = throwError e
liftResult path Fail = error $ "Fatal error parsing " ++ path

resultToEither :: FilePath -> Result e a -> Either e a
resultToEither _ (OK res _ _) = Right res
resultToEither _ (Err e) = Left e
resultToEither path Fail = error $ "Fatal error parsing " ++ path

parseMaybe :: r -> FP.Parser r e a -> ByteString -> Maybe a
parseMaybe r p s = case runParser p r 0 s of
  OK res _ _ -> Just res
  Err _ -> Nothing
  Fail -> Nothing

count ::
  Int ->
  FP.Parser r e a ->
  FP.Parser r e [a]
count cnt p = go cnt
  where
    go 0 = pure []
    go n = (:) <$> p <*> go (pred n)

between ::
  FP.Parser r e () ->
  FP.Parser r e () ->
  FP.Parser r e a ->
  FP.Parser r e a
between s e p = s *> p <* e

endBy1 ::
  FP.Parser r e a ->
  FP.Parser r e sep ->
  FP.Parser r e [a]
endBy1 p sep = some $ do
  x <- p
  _ <- sep
  return x

sepBy1 ::
  FP.Parser r e a ->
  FP.Parser r e sep ->
  FP.Parser r e [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

manyTill ::
  (Show a) =>
  FP.Parser r e a ->
  FP.Parser r e () ->
  FP.Parser r e [a]
manyTill p e = go []
  where
    go acc =
      (reverse acc <$ e)
        <|> (go . (: acc) =<< p)

manyTill_ ::
  FP.Parser r e a ->
  FP.Parser r e end ->
  FP.Parser r e ([a], end)
manyTill_ p e = go []
  where
    go !acc = do
      (let !y = reverse acc in ((y,) <$> e))
        <|> (go . (: acc) =<< p)

someTill ::
  FP.Parser r e a ->
  FP.Parser r e () ->
  FP.Parser r e [a]
someTill p e = go []
  where
    go acc = do
      x <- p
      ((x : acc) <$ e)
        <|> go (x : acc)

newline :: FP.Parser r e ()
newline = $(char '\n')

singleSpace :: FP.Parser r e ()
singleSpace = $(char ' ')

spaces_ :: FP.Parser r e ()
spaces_ = skipSome singleSpace

digitChar :: FP.Parser r e Char
digitChar = satisfy isDigit

trailingSpace :: FP.Parser r e ()
trailingSpace = skipMany singleSpace <* newline

lineOrEof :: FP.Parser r e String
lineOrEof = takeLine

wholeLine :: FP.Parser r e String
wholeLine = takeLine

restOfLine :: FP.Parser r e String
restOfLine = takeLine

identifier :: FP.Parser r e String
identifier = some (satisfy (\c -> isAlphaNum c || c == '_' || c == ' '))
