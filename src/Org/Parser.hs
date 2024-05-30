{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Org.Parser where

import Control.Applicative
import Control.Arrow (first, left, second)
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Char (isAlphaNum, isPrint, isSpace)
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

spaces_ :: Parser ()
spaces_ = skipSome singleSpace

trailingSpace :: Parser ()
trailingSpace = skipManyTill singleSpace (void newline)

anyChar :: Parser Char
anyChar = satisfy $ \c -> c /= '\n'

newlineOrEof :: Parser ()
newlineOrEof = void newline <|> eof

lineOrEof :: Parser Text
lineOrEof = pack <$> manyTill anyChar newlineOrEof

line :: Parser Text
line = pack <$> manyTill anyChar newline

restOfLine :: Parser Text
restOfLine = pack <$> someTill anyChar newline

identifier :: (MonadParsec e s m, Token s ~ Char) => m Text
identifier = pack <$> many (alphaNumChar <|> char '_')

parseOrgFile :: Parser OrgFile
parseOrgFile = do
  SourcePos path _ _ <- getSourcePos
  OrgFile path <$> parseHeader <*> many (parseEntry 1)

parseProperties :: Parser [Property]
parseProperties = do
  string ":PROPERTIES:" *> trailingSpace
  -- RULE: Property blocks are never empty
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
parseHeader = do
  _headerPropertiesDrawer <-
    join . maybeToList <$> optional (try parseProperties)
  _headerFileProperties <- many parseFileProperty
  SourcePos path _ _ <- getSourcePos
  cfg <- ask
  let _headerTitle =
        lookupProperty False _headerFileProperties "title"
  _headerTags <- do
    case lookupProperty False _headerFileProperties "filetags" of
      Nothing -> pure []
      Just filetags -> do
        case left
          errorBundlePretty
          (runReader (runParserT (parseTags <* eof) path filetags) cfg) of
          Left err -> fail err
          Right x -> pure x
  _headerPreamble <- parseEntryBody
  pure Header {..}

parseHeaderStars :: Parser Int
parseHeaderStars = length <$> someTill (char '*') (some singleSpace)

parseFileProperty :: Parser Property
parseFileProperty = do
  _name <- between (string "#+") (char ':') identifier
  skipMany singleSpace
  _value <- restOfLine
  let _inherited = False
  pure Property {..}

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
  _entryHeadline <- lookAhead (try restOfLine)
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
  logEntries <- many parseLogEntry
  text <- parseEntryBody
  let (_entryLogEntries, _entryText) =
        let dflt = (logEntries, text)
         in case reverse logEntries of
              [] -> dflt
              l : ls -> case l ^? _LogBody of
                Nothing -> dflt
                Just bdy -> case reverse (_blocks bdy) of
                  (le@(Whitespace _) : les) ->
                    case ( _blocks text,
                           reverse (_blocks text)
                         ) of
                      (b : bs, Whitespace _ : _) ->
                        ( reverse ((l & _LogBody .~ Body (reverse les)) : ls),
                          Body (le : b : bs)
                        )
                      _ -> dflt
                  _ -> dflt
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
parseTags = colon *> sepBy1 parseTag colon
  where
    colon = char ':'
    tag :: (MonadParsec e s m, Token s ~ Char) => m String
    tag =
      many
        ( satisfy $ \ch ->
            isAlphaNum ch
              || ch `elem` ['-', '_', '=', '/']
        )
    parseTag = do
      nm <- pack <$> tag
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

parseLogHeading :: Parser (Maybe Body -> LogEntry)
parseLogHeading =
  parseClosing
    <|> parseState
    <|> parseNote
    <|> parseRescheduled
    <|> parseNotScheduled
    <|> parseDeadline
    <|> parseNoDeadline
    <|> parseRefiling
  where
    parseClosing = do
      _ <- try (string "- CLOSING NOTE" <* spaces_)
      LogClosing <$> parseTimeSingle

    parseState = do
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
      LogState fromKeyword toKeyword <$> parseTimeSingle

    parseNote = do
      _ <- try (string "- Note taken on" <* spaces_) --
      LogNote <$> parseTimeSingle

    parseRescheduled = do
      _ <- try (string "- Rescheduled from \"")
      origTime <- parseTimeSingle
      _ <- string "\" on" <* spaces_
      LogRescheduled origTime <$> parseTimeSingle

    parseNotScheduled = do
      _ <- try (string "- Not scheduled, was \"")
      origTime <- parseTimeSingle
      _ <- string "\" on" <* spaces_
      LogNotScheduled origTime <$> parseTimeSingle

    parseDeadline = do
      _ <- try (string "- New deadline from \"")
      origTime <- parseTimeSingle
      _ <- string "\" on" <* spaces_
      LogDeadline origTime <$> parseTimeSingle

    parseNoDeadline = do
      _ <- try (string "- Removed deadline, was \"")
      origTime <- parseTimeSingle
      _ <- string "\" on" <* spaces_
      LogNoDeadline origTime <$> parseTimeSingle

    parseRefiling = do
      _ <- try (string "- Refiled on" <* spaces_)
      LogRefiling <$> parseTimeSingle

parseLogEntry :: Parser LogEntry
parseLogEntry =
  (parseLogHeading >>= (<$> trailingNote))
    <|> parseClockEntry
    <|> parseLogBook
  where
    trailingNote =
      try spaces_
        *> string "\\\\"
        *> trailingSpace
        *> (Just <$> parseNoteBody)
        <|> (Nothing <$ try trailingSpace)

    parseClockEntry = do
      _ <- try (string "CLOCK:" <* spaces_)
      start <- parseTimeSingle
      mend <- optional $ try $ do
        _ <- string "--"
        end <- parseTimeSingle
        let tm = blendTimes start end
        _ <- spaces_ *> string "=>" *> spaces_
        _hours <- read <$> some digitChar
        _ <- char ':'
        _mins <- read <$> some digitChar
        trailingSpace
        pure (tm, Duration {..})
      pure $
        maybe
          (LogClock start Nothing)
          (uncurry LogClock . second Just)
          mend

    parseLogBook = do
      _ <- try (string ":LOGBOOK:") <* trailingSpace
      book <- many parseLogEntry
      string ":END:" *> trailingSpace
      return $ LogBook book

parseEntryBody :: Parser Body
parseEntryBody = parseBody (pure ()) parseHeaderStars

parseNoteBody :: Parser Body
parseNoteBody =
  parseBody
    (try (void (string "  ")) <|> lookAhead (void newline))
    (satisfy (not . isSpace))

parseBody :: Parser a -> Parser b -> Parser Body
parseBody leader terminus =
  mconcat
    <$> manyTill
      (Body . (: []) <$> parseBlock leader)
      (try (void (lookAhead terminus)) <|> eof)

parseBlock :: Parser a -> Parser Block
parseBlock leader = do
  (Whitespace . T.pack <$> try (manyTill singleSpace newline))
    <|> Drawer <$> try (leader *> parseDrawer leader)
    <|> Paragraph . (: []) <$> (leader *> lineOrEof)

parseDrawer :: Parser a -> Parser [Text]
parseDrawer leader = parseDrawerBlock <|> parseBeginBlock
  where
    parseDrawerBlock = do
      txt <- try $ do
        prefix <- T.pack <$> many singleSpace
        _ <- char ':'
        ident <- identifier
        _ <- char ':'
        trailingSpace
        pure $ prefix <> ":" <> ident <> ":"
      content <-
        manyTill
          (leader *> restOfLine <|> T.singleton <$> newline)
          ( lookAhead
              ( try
                  ( void
                      ( leader
                          *> skipMany singleSpace
                          *> string' ":end:"
                      )
                  )
                  <|> eof
              )
          )
      endLine <- do
        _ <- leader
        prefix <- T.pack <$> many singleSpace
        ending <- string' ":end:"
        trailingSpace
        pure $ prefix <> ending
      pure $ txt : content ++ [endLine]

    parseBeginBlock = do
      txt <- try $ do
        prefix <- T.pack <$> many singleSpace
        begin <- string' "#+begin"
        suffix <- line
        pure $ prefix <> begin <> suffix
      content <-
        manyTill
          (leader *> restOfLine <|> T.singleton <$> newline)
          ( lookAhead
              ( try
                  ( void
                      ( leader
                          *> skipMany singleSpace
                          *> string' "#+end"
                      )
                  )
                  <|> eof
              )
          )
      endLine <- do
        _ <- leader
        prefix <- T.pack <$> many singleSpace
        ending <- string' "#+end"
        suffix <- line
        pure $ prefix <> ending <> suffix
      pure $ txt : content ++ [endLine]
