{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Parser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.Functor.Identity
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Time
import Data.Void
import Debug.Trace
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

type Parser = ParsecT Void Text Identity

data OrgFile = OrgFile
  { fileHeader :: OrgHeader,
    fileEntries :: [OrgEntry],
    fileEndProperties :: [Property]
  }
  deriving (Show)

parseOrg :: Parser OrgFile
parseOrg =
  OrgFile
    <$> parseHeader
    <*> many parseEntry
    <*> many parseFileProperty

data Property = Property
  { propertyName :: Text,
    propertyValue :: Text
  }
  deriving (Show)

tabOrSpace :: Parser Char
tabOrSpace = char ' ' <|> char '\t'

trailingSpace :: Parser ()
trailingSpace = do
  traceM "trailingSpace"
  skipManyTill tabOrSpace (void newline)

restOfLine :: Parser Text
restOfLine = pack <$> someTill (printChar <|> tabOrSpace) (try trailingSpace)

identifier :: Parser Text
identifier = pack <$> many (alphaNumChar <|> char '_')

parseProperties :: Parser [Property]
parseProperties = do
  traceM "parseProperties"
  string ":PROPERTIES:" *> trailingSpace
  props <- some $ try $ do
    propertyName <- between (char ':') (char ':') identifier
    guard $ propertyName /= "END"
    skipMany tabOrSpace
    propertyValue <- restOfLine
    pure Property {..}
  string ":END:" *> trailingSpace
  return props

data OrgHeader = OrgHeader
  { headerPropertiesDrawer :: [Property],
    headerFileProperties :: [Property],
    headerPreamble :: Maybe [Text]
  }
  deriving (Show)

parseHeader :: Parser OrgHeader
parseHeader = do
  traceM "parseHeader"
  OrgHeader
    <$> (join . maybeToList <$> optional parseProperties)
    <*> many parseFileProperty
    <*> parseText

parseFileProperty :: Parser Property
parseFileProperty = do
  propertyName <- between (string "#+") (char ':') identifier
  skipMany tabOrSpace
  propertyValue <- restOfLine
  pure Property {..}

line :: Parser Text
line = pack <$> manyTill (printChar <|> tabOrSpace) (try trailingSpace)

parseText :: Parser (Maybe [Text])
parseText = do
  traceM "parseText"
  optional (someTill line (lookAhead (char '*')))

data OrgEntry = OrgEntry
  { entryDepth :: Int,
    entryKeyword :: Maybe Text,
    entryPriority :: Maybe Text,
    entryTitle :: Text,
    entryLocation :: Maybe Text,
    entryTags :: [Text],
    entryClosed :: Maybe UTCTime,
    entryScheduled :: Maybe UTCTime,
    entryDeadline :: Maybe UTCTime,
    entryDated :: Maybe (UTCTime, Maybe UTCTime),
    entryProperties :: [Property],
    entryText :: Maybe [Text]
  }
  deriving (Show)

parseEntry :: Parser OrgEntry
parseEntry = do
  traceM "parseEntry"
  entryDepth <- length <$> someTill (char '*') (some tabOrSpace)
  traceM "parseEntry..1"
  entryKeyword <-
    optional $
      ( string "TODO"
          <|> string "CATEGORY"
          <|> string "DONE"
      )
        <* some tabOrSpace
  traceM "parseEntry..2"
  entryPriority <-
    optional $ string "[#" *> (pack . (: []) <$> oneOf ['A' .. 'C']) <* char ']'
  traceM "parseEntry..3"
  (entryTitle, afterTitle) <-
    first pack
      <$> manyTill_
        (printChar <|> tabOrSpace)
        ( try
            ( some tabOrSpace
                *> ( (Just .) . T.cons
                       <$> (char '{' <|> char ':')
                       <*> restOfLine
                   )
                <|> (Nothing <$ trailingSpace)
            )
        )
  traceM "parseEntry..4"
  entryLocation <- pure Nothing
  traceM "parseEntry..5"
  let entryTags = join $ maybeToList $ do
        after <- afterTitle
        parseMaybe
          (between (char ':') (char ':') (sepBy identifier (char ':')))
          after
  traceM "parseEntry..6"
  entryClosed <- pure Nothing
  traceM "parseEntry..8"
  entryScheduled <- pure Nothing
  traceM "parseEntry..9"
  entryDeadline <- pure Nothing
  traceM "parseEntry..10"
  entryDated <- pure Nothing
  traceM "parseEntry..11"
  entryProperties <- join . maybeToList <$> optional parseProperties
  traceM "parseEntry..12"
  entryText <-
    (Nothing <$ lookAhead (void (char '*') <|> void (string "#+")))
      <|> parseText
  traceM "parseEntry..13"
  pure OrgEntry {..}
