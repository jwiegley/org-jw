{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Org.Parser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Data.Char (isPrint, isSpace)
import Data.Maybe (isJust, maybeToList)
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy qualified as T
import Data.Time
import Org.Types
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

oneOfList :: (MonadParsec e s m) => [Tokens s] -> m (Tokens s)
oneOfList = foldr (\x rest -> string x <|> rest) mzero

singleSpace :: Parser Char
singleSpace = char ' '

spaces_ :: Parser ()
spaces_ = skipSome singleSpace

trailingSpace :: Parser ()
trailingSpace = skipManyTill singleSpace (void newline)

anyChar :: Parser Char
anyChar = satisfy $ \c -> c /= '\n'

newlineOrEof :: Parser ()
newlineOrEof = void newline <|> eof

line :: Parser Text
line = pack <$> manyTill anyChar newlineOrEof

restOfLine :: Parser Text
restOfLine = pack <$> someTill anyChar newline

identifier :: (MonadParsec e s m, Token s ~ Char) => m Text
identifier = pack <$> many (alphaNumChar <|> char '_')

parseOrgFile :: Parser OrgFile
parseOrgFile = OrgFile <$> parseHeader <*> many (parseEntry 1)

parseProperties :: Parser [Property]
parseProperties = do
  string ":PROPERTIES:" *> trailingSpace
  props <- some $ try $ do
    _name <- between (char ':') (char ':') identifier
    guard $ _name /= "END"
    skipMany singleSpace
    _value <- restOfLine
    let _inherited = False
    pure Property {..}
  string ":END:" *> trailingSpace
  return props

parseHeader :: Parser Header
parseHeader =
  (Header . join . maybeToList <$> optional (try parseProperties))
    <*> many parseFileProperty
    <*> parseEntryText

parseFileProperty :: Parser Property
parseFileProperty = do
  _name <- between (string "#+") (char ':') identifier
  skipMany singleSpace
  _value <- restOfLine
  let _inherited = False
  pure Property {..}

parseHeaderStars :: Parser Int
parseHeaderStars = length <$> someTill (char '*') (some singleSpace)

parseEntryText :: Parser [Text]
parseEntryText = manyTill line (try (void (lookAhead parseHeaderStars)) <|> eof)

parseKeyword :: Parser Keyword
parseKeyword = do
  Config {..} <- ask
  OpenKeyword <$> oneOfList _openKeywords
    <|> ClosedKeyword <$> oneOfList _closedKeywords

parseEntry :: Int -> Parser Entry
parseEntry parseAtDepth = do
  SourcePos _entryFile (unPos -> _entryLine) (unPos -> _entryColumn) <-
    getSourcePos
  _entryDepth <- try $ do
    depth <- parseHeaderStars
    guard $ depth == parseAtDepth
    pure depth
  _entryKeyword <- optional (try parseKeyword <* some singleSpace)
  _entryPriority <- optional parseEntryPriority
  _entryContext <- optional parseEntryContext
  (_entryTitle, (_entryLocator, _entryTags)) <-
    first pack <$> manyTill_ anyChar (try parseTitleSuffix)
  _entryStamps <-
    join . maybeToList
      <$> try (optional (parseStamps <* trailingSpace))
  _entryProperties <-
    join . maybeToList
      <$> try (optional parseProperties)
  _entryLogEntries <- many parseLogEntry
  _entryText <- parseEntryText
  _entryItems <- many (parseEntry (succ _entryDepth))
  pure Entry {..}

parseEntryPriority :: Parser Text
parseEntryPriority = do
  Config {..} <- ask
  prio <- string "[#" *> oneOfList _priorities <* char ']'
  spaces_
  pure prio

parseEntryContext :: Parser Text
parseEntryContext = do
  context <-
    between
      (char '(')
      (char ')')
      (pack <$> some (satisfy (\c -> c /= ')' && (isPrint c || c == ' '))))
  spaces_
  pure context

parseTitleSuffix :: Parser (Maybe Text, [Tag])
parseTitleSuffix =
  ((Nothing, []) <$ try trailingSpace)
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

parseStamps :: Parser [Stamp]
parseStamps = sepBy1 parseStamp (char ' ')

parseStamp :: Parser Stamp
parseStamp =
  string "CLOSED"
    *> string ": "
    *> (ClosedStamp <$> parseTimeSingle)
    <|> string "SCHEDULED"
      *> string ": "
      *> (ScheduledStamp <$> parseTimeSingle)
    <|> string "DEADLINE"
      *> string ": "
      *> (DeadlineStamp <$> parseTimeSingle)

blendTimes :: Time -> Time -> Time
blendTimes start Time {..} =
  start
    { _timeDayEnd = Just _timeDay,
      _timeEnd = _timeStart
    }

parseTime ::
  (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), MonadFail m) =>
  m Time
parseTime = do
  start <- parseTimeSingle
  mend <- optional $ string "--" *> parseTimeSingle
  case mend of
    Nothing -> pure start
    Just tm@Time {..} -> do
      forM_ _timeDayEnd $ \_ ->
        fail $ "Invalid org time: " ++ show tm
      forM_ _timeEnd $ \_ ->
        fail $ "Invalid org time: " ++ show tm
      forM_ _timeSuffix $ \_ ->
        fail $ "Invalid org time: " ++ show tm
      pure $ blendTimes start tm

parseTimeSingle ::
  (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), MonadFail m) =>
  m Time
parseTimeSingle = do
  _timeKind <-
    ActiveTime <$ char '<'
      <|> InactiveTime <$ char '['
  year <- count 4 numberChar
  _ <- char '-'
  month <- count 2 numberChar
  _ <- char '-'
  day <- count 2 numberChar
  _timeDay <- case fromGregorianValid (read year) (read month) (read day) of
    Just d -> pure $ fromInteger $ toModifiedJulianDay d
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
    pure $ read hour * 60 + read minute
  _timeEnd <- optional $ do
    guard $ isJust _timeStart
    _ <- char '-'
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ read hour * 60 + read minute
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
    _suffixLargerSpan <- optional $ try $ do
      _ <- char '/'
      num <- read <$> some digitChar
      s <-
        MonthSpan <$ char 'm'
          <|> DaySpan <$ char 'd'
          <|> WeekSpan <$ char 'w'
      pure (num, s)
    pure TimeSuffix {..}
  _ <- case _timeKind of
    ActiveTime -> char '>'
    InactiveTime -> char ']'
  pure Time {..}

parseLogEntry :: Parser LogEntry
parseLogEntry = parseStateChange <|> parseNote <|> parseLogBook
  where
    trailingNote =
      ([] <$ try (skipSome trailingSpace))
        <|> spaces_
          *> string "\\\\"
          *> trailingSpace
          *> parseNoteText

    parseStateChange = do
      fromKeyword <-
        try (string "- State \"")
          *> parseKeyword
          <* char '"'
          <* spaces_
      toKeyword <-
        optional $
          try $
            string "from"
              *> spaces_
              *> char '"'
              *> parseKeyword
              <* char '"'
              <* spaces_
      LogState fromKeyword toKeyword <$> parseTimeSingle <*> trailingNote

    parseNote = do
      _ <- try (string "- Note taken on" <* spaces_) --
      LogNote <$> parseTimeSingle <*> trailingNote

    parseLogBook = do
      _ <- try (string ":LOGBOOK:") <* trailingSpace
      book <- many $ do
        _ <- string "CLOCK:" <* spaces_
        start <- parseTimeSingle
        _ <- string "--"
        end <- parseTimeSingle
        let tm = blendTimes start end
        _ <- spaces_ *> string "=>" *> spaces_
        _hours <- read <$> some digitChar
        _ <- char ':'
        _mins <- read <$> some digitChar
        trailingSpace
        pure (tm, Duration {..})
      string ":END:" *> trailingSpace
      return $ LogBook book

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
      "Unexpected log entry: " ++ show xs ++ ", leaders: " ++ show leaders
  pure $ map (T.drop 2) xs
