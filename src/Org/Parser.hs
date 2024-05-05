{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Parser (parseOrg) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Data.Char (isPrint)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text, pack)
import Data.Time
import Org.Types
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

oneOfList :: (MonadParsec e s m) => [Tokens s] -> m (Tokens s)
oneOfList = foldr (\x rest -> string x <|> rest) mzero

singleSpace :: Parser Char
singleSpace = char ' '

trailingSpace :: Parser ()
trailingSpace = skipManyTill singleSpace (void newline)

line :: Parser Text
line = pack <$> manyTill (printChar <|> singleSpace) newline

restOfLine :: Parser Text
restOfLine = pack <$> someTill (printChar <|> singleSpace) newline

identifier :: Parser Text
identifier = pack <$> many (alphaNumChar <|> char '_')

parseOrg :: Parser OrgFile
parseOrg = OrgFile <$> parseHeader <*> many (parseEntry 1)

parseProperties :: Parser [Property]
parseProperties = do
  string ":PROPERTIES:" *> trailingSpace
  props <- some $ try $ do
    propertyName <- between (char ':') (char ':') identifier
    guard $ propertyName /= "END"
    skipMany singleSpace
    propertyValue <- restOfLine
    pure Property {..}
  string ":END:" *> trailingSpace
  return props

parseHeader :: Parser OrgHeader
parseHeader =
  OrgHeader
    <$> (join . maybeToList <$> optional (try parseProperties))
    <*> many parseFileProperty
    <*> (listToMaybeList <$> parseText)

parseFileProperty :: Parser Property
parseFileProperty = do
  propertyName <- between (string "#+") (char ':') identifier
  skipMany singleSpace
  propertyValue <- restOfLine
  pure Property {..}

parseHeaderStars :: Parser Int
parseHeaderStars = length <$> someTill (char '*') (some singleSpace)

parseText :: Parser [Text]
parseText = manyTill line (try (void (lookAhead parseHeaderStars)) <|> eof)

parseEntry :: Int -> Parser OrgEntry
parseEntry parseAtDepth = do
  entryDepth <- try $ do
    depth <- parseHeaderStars
    guard $ depth == parseAtDepth
    pure depth
  OrgConfig {..} <- ask
  entryKeyword <-
    optional
      ( try
          ( OpenKeyword <$> oneOfList openKeywords
              <|> ClosedKeyword <$> oneOfList closedKeywords
          )
          <* some singleSpace
      )
  entryPriority <- optional $ do
    guard $ isJust entryKeyword
    parseEntryPriority
  entryContext <- optional parseEntryContext
  (entryTitle, (entryLocator, entryTags)) <-
    first pack
      <$> manyTill_ (printChar <|> singleSpace) (try parseTitleSuffix)
  entryStamps <-
    join . maybeToList
      <$> try (optional (parseOrgStamps <* trailingSpace))
  entryProperties <-
    join . maybeToList
      <$> try (optional parseProperties)
  entryText <- listToMaybeList <$> parseText
  forM_ entryText $ \txt ->
    when (":PROPERTIES:" `elem` txt) $
      fail $
        "Floating properties drawer in: " ++ show txt
  entryItems <- many (parseEntry (succ entryDepth))
  pure OrgEntry {..}

parseEntryPriority :: Parser Text
parseEntryPriority = do
  OrgConfig {..} <- ask
  prio <- string "[#" *> oneOfList priorities <* char ']'
  skipSome singleSpace
  pure prio

parseEntryContext :: Parser Text
parseEntryContext = do
  context <-
    between
      (char '(')
      (char ')')
      (pack <$> some (satisfy (\c -> c /= ')' && (isPrint c || c == ' '))))
  skipSome singleSpace
  pure context

parseTitleSuffix :: Parser (Maybe Text, [OrgTag])
parseTitleSuffix =
  ((Nothing, []) <$ try (skipManyTill singleSpace newline))
    <|> do
      _ <- some singleSpace
      location <- optional (try parseLocation)
      tags <-
        optional
          ( try
              ( ( case location of
                    Nothing -> pure ()
                    Just _ -> void $ singleSpace *> some singleSpace
                )
                  *> parseTags
              )
          )
      trailingSpace
      pure (location, join (maybeToList tags))

parseLocation :: Parser Text
parseLocation = between (char '{') (char '}') identifier

parseTags :: Parser [OrgTag]
parseTags =
  filter
    ( \case
        OrgPlainTag "" -> False
        _ -> True
    )
    <$> (char ':' *> sepBy1 parseTag (char ':'))
  where
    parseTag = do
      name <- identifier
      OrgConfig {..} <- ask
      pure $
        if name `elem` specialTags
          then OrgSpecialTag name
          else OrgPlainTag name

parseOrgStamps :: Parser [OrgStamp]
parseOrgStamps = sepBy1 parseOrgStamp (char ' ')

parseOrgStamp :: Parser OrgStamp
parseOrgStamp = do
  orgStampKind <-
    ClosedStamp <$ string "CLOSED"
      <|> ScheduledStamp <$ string "SCHEDULED"
      <|> DeadlineStamp <$ string "DEADLINE"
  _ <- string ": "
  orgStampTime <- parseOrgTimeSingle
  case orgStampKind of
    ClosedStamp
      | orgTimeKind orgStampTime == ActiveTime ->
          fail $ "Closed stamps must use inactive times"
    ScheduledStamp
      | orgTimeKind orgStampTime == InactiveTime ->
          fail $ "Scheduled stamps must use active times"
    DeadlineStamp
      | orgTimeKind orgStampTime == InactiveTime ->
          fail $ "Deadline stamps must use active times"
    _ -> pure ()
  pure OrgStamp {..}

{-
parseOrgTime :: Parser OrgTime
parseOrgTime = do
  start <- parseOrgTimeSingle
  mend <- optional $ string "--" *> parseOrgTimeSingle
  case mend of
    Nothing -> pure start
    Just ts@OrgTime {..} -> do
      forM_ orgTimeDayEnd $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      forM_ orgTimeEnd $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      forM_ orgTimeSuffix $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      pure
        start
          { orgTimeDayEnd = Just orgTimeDay,
            orgTimeEnd = orgTimeStart
          }
-}

parseOrgTimeSingle :: Parser OrgTime
parseOrgTimeSingle = do
  orgTimeKind <-
    ActiveTime <$ char '<'
      <|> InactiveTime <$ char '['
  year <- count 4 numberChar
  _ <- char '-'
  month <- count 2 numberChar
  _ <- char '-'
  day <- count 2 numberChar
  orgTimeDay <- case fromGregorianValid (read year) (read month) (read day) of
    Just d -> pure d
    Nothing ->
      fail $
        "Could not parse gregorian date: "
          ++ year
          ++ "-"
          ++ month
          ++ "-"
          ++ day
  let orgTimeDayEnd = Nothing
  _ <- char ' '
  _dow <- oneOfList ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  orgTimeStart <- optional $ try $ do
    _ <- char ' '
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  orgTimeEnd <- optional $ do
    guard $ isJust orgTimeStart
    _ <- char '-'
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  orgTimeSuffix <- optional $ do
    _ <- char ' '
    repeatDotted <- isJust <$> optional (char '.')
    orgSuffixKind <-
      ( if repeatDotted
          then OrgTimeDottedRepeat
          else OrgTimeRepeat
        )
        <$ char '+'
        <|> OrgTimeWithin <$ char '-'
    orgSuffixNum <- read <$> some digitChar
    orgSuffixSpan <-
      OrgMonthSpan <$ char 'm'
        <|> OrgDaySpan <$ char 'd'
        <|> OrgWeekSpan <$ char 'w'
    pure OrgTimeSuffix {..}
  _ <- case orgTimeKind of
    ActiveTime -> char '>'
    InactiveTime -> char ']'
  pure OrgTime {..}
