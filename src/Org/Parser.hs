{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Org.Parser (parseOrg, parseOrgTime, parseOrgTimeSingle) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Data.Char (isPrint, isSpace)
import Data.Maybe (isJust, maybeToList)
import Data.String
import Data.Text (Text, pack)
import Data.Text qualified as T
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
    _name <- between (char ':') (char ':') identifier
    guard $ _name /= "END"
    skipMany singleSpace
    _value <- restOfLine
    pure Property {..}
  string ":END:" *> trailingSpace
  return props

parseHeader :: Parser Header
parseHeader =
  Header
    <$> (join . maybeToList <$> optional (try parseProperties))
    <*> many parseFileProperty
    <*> parseEntryText

parseFileProperty :: Parser Property
parseFileProperty = do
  _name <- between (string "#+") (char ':') identifier
  skipMany singleSpace
  _value <- restOfLine
  pure Property {..}

parseHeaderStars :: Parser Int
parseHeaderStars = length <$> someTill (char '*') (some singleSpace)

parseEntryText :: Parser [Text]
parseEntryText = manyTill line (try (void (lookAhead parseHeaderStars)) <|> eof)

parseOrgKeyword :: Parser Keyword
parseOrgKeyword = do
  Config {..} <- ask
  OpenKeyword <$> oneOfList _openKeywords
    <|> ClosedKeyword <$> oneOfList _closedKeywords

parseEntry :: Int -> Parser Entry
parseEntry parseAtDepth = do
  _entryPos <- getSourcePos
  _entryDepth <- try $ do
    depth <- parseHeaderStars
    guard $ depth == parseAtDepth
    pure depth
  _entryKeyword <- optional (try parseOrgKeyword <* some singleSpace)
  _entryPriority <- optional $ do
    guard $ isJust _entryKeyword
    parseEntryPriority
  _entryContext <- optional parseEntryContext
  (_entryTitle, (_entryLocator, _entryTags)) <-
    first pack
      <$> manyTill_ (printChar <|> singleSpace) (try parseTitleSuffix)
  _entryStamps <-
    join . maybeToList
      <$> try (optional (parseOrgStamps <* trailingSpace))
  _entryProperties <-
    join . maybeToList
      <$> try (optional parseProperties)
  _entryLogEntries <- many parseLogEntry
  _entryText <- parseEntryText
  forM_ _entryText $ \txt ->
    when (":PROPERTIES:" `T.isInfixOf` txt) $
      fail $
        "Floating properties drawer in: " ++ show txt
  _entryItems <- many (parseEntry (succ _entryDepth))
  pure Entry {..}

parseEntryPriority :: Parser Text
parseEntryPriority = do
  Config {..} <- ask
  prio <- string "[#" *> oneOfList _priorities <* char ']'
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

parseTitleSuffix :: Parser (Maybe Text, [Tag])
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

parseTags :: Parser [Tag]
parseTags =
  filter
    ( \case
        PlainTag "" -> False
        _ -> True
    )
    <$> (char ':' *> sepBy1 parseTag (char ':'))
  where
    parseTag = do
      nm <- identifier
      Config {..} <- ask
      pure $
        if nm `elem` _specialTags
          then SpecialTag nm
          else PlainTag nm

parseOrgStamps :: Parser [Stamp]
parseOrgStamps = sepBy1 parseOrgStamp (char ' ')

parseOrgStamp :: Parser Stamp
parseOrgStamp = do
  orgStampKind <-
    ClosedStamp <$ string "CLOSED"
      <|> ScheduledStamp <$ string "SCHEDULED"
      <|> DeadlineStamp <$ string "DEADLINE"
  _ <- string ": "
  orgStampTime <- parseOrgTimeSingle
  case orgStampKind of
    ClosedStamp
      | _timeKind orgStampTime == ActiveTime ->
          fail $ "Closed stamps must use inactive times"
    ScheduledStamp
      | _timeKind orgStampTime == InactiveTime ->
          fail $ "Scheduled stamps must use active times"
    DeadlineStamp
      | _timeKind orgStampTime == InactiveTime ->
          fail $ "Deadline stamps must use active times"
    _ -> pure ()
  pure Stamp {..}

parseOrgTime :: Parser Time
parseOrgTime = do
  start <- parseOrgTimeSingle
  mend <- optional $ string "--" *> parseOrgTimeSingle
  case mend of
    Nothing -> pure start
    Just ts@Time {..} -> do
      forM_ _timeDayEnd $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      forM_ _timeEnd $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      forM_ _timeSuffix $ \_ ->
        fail $ "Invalid org time: " ++ show ts
      pure
        start
          { _timeDayEnd = Just _timeDay,
            _timeEnd = _timeStart
          }

parseOrgTimeSingle ::
  (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), MonadFail m) =>
  m Time
parseOrgTimeSingle = do
  _timeKind <-
    ActiveTime <$ char '<'
      <|> InactiveTime <$ char '['
  year <- count 4 numberChar
  _ <- char '-'
  month <- count 2 numberChar
  _ <- char '-'
  day <- count 2 numberChar
  _timeDay <- case fromGregorianValid (read year) (read month) (read day) of
    Just d -> pure d
    Nothing ->
      fail $
        "Could not parse gregorian date: "
          ++ year
          ++ "-"
          ++ month
          ++ "-"
          ++ day
  let _timeDayEnd = Nothing
  _ <- char ' '
  _dow <- oneOfList ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  _timeStart <- optional $ try $ do
    _ <- char ' '
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  _timeEnd <- optional $ do
    guard $ isJust _timeStart
    _ <- char '-'
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  _timeSuffix <- optional $ do
    _ <- char ' '
    repeatDotted <- isJust <$> optional (char '.')
    _suffixKind <-
      ( if repeatDotted
          then TimeDottedRepeat
          else TimeRepeat
        )
        <$ char '+'
        <|> TimeWithin <$ char '-'
    _suffixNum <- read <$> some digitChar
    _suffixSpan <-
      MonthSpan <$ char 'm'
        <|> DaySpan <$ char 'd'
        <|> WeekSpan <$ char 'w'
    pure TimeSuffix {..}
  _ <- case _timeKind of
    ActiveTime -> char '>'
    InactiveTime -> char ']'
  pure Time {..}

parseLogEntry :: Parser LogEntry
parseLogEntry = parseStateChange <|> parseNote
  where
    parseStateChange = do
      _ <- string "- State \""
      fromKeyword <- parseOrgKeyword
      _ <- char '"'
      skipManyTill singleSpace (void (string "from \""))
      toKeyword <- parseOrgKeyword
      _ <- char '"'
      skipSome singleSpace
      logTime <- parseOrgTimeSingle
      logNote <-
        [] <$ try newline
          <|> skipSome singleSpace *> string "\\\\" *> newline *> parseNoteText
      pure $ LogStateChange fromKeyword toKeyword logTime logNote

    parseNote = do
      _ <- string "- Note take on "
      logTime <- parseOrgTimeSingle
      logNote <-
        [] <$ newline
          <|> skipSome singleSpace *> string "\\\\" *> newline *> parseNoteText
      pure $ LogNote logTime logNote

parseNoteText :: Parser [Text]
parseNoteText = do
  xs <-
    manyTill
      line
      ( try (void (lookAhead (satisfy (not . isSpace))))
          <|> eof
      )
  let leaders = filter (\x -> x /= "  " && x /= "") $ map (T.take 2) xs
  unless (null leaders) $
    fail $
      "Unexpected log entry: " ++ show xs
  pure $ map (T.drop 2) xs
