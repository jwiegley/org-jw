{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Parser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader
import Data.Char (isPrint)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Time
import Data.Void
import Debug.Trace
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

data OrgConfig = OrgConfig
  { openKeywords :: [Text],
    closedKeywords :: [Text]
  }
  deriving (Show)

type Parser = ParsecT Void Text (Reader OrgConfig)

data OrgFile = OrgFile
  { fileHeader :: OrgHeader,
    fileEntries :: [OrgEntry]
  }
  deriving (Show)

parseOrg :: Parser OrgFile
parseOrg =
  OrgFile
    <$> parseHeader
    <*> many parseEntry

data Property = Property
  { propertyName :: Text,
    propertyValue :: Text
  }
  deriving (Show)

singleSpace :: Parser Char
singleSpace = char ' '

trailingSpace :: Parser ()
trailingSpace = do
  traceM "trailingSpace"
  skipManyTill singleSpace (void newline)

restOfLine :: Parser Text
restOfLine = pack <$> someTill (printChar <|> singleSpace) (try trailingSpace)

identifier :: Parser Text
identifier = pack <$> many (alphaNumChar <|> char '_')

parseProperties :: Parser [Property]
parseProperties = do
  traceM "parseProperties"
  string ":PROPERTIES:" *> trailingSpace
  props <- some $ try $ do
    propertyName <- between (char ':') (char ':') identifier
    guard $ propertyName /= "END"
    skipMany singleSpace
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
    <$> (join . maybeToList <$> optional (try parseProperties))
    <*> many parseFileProperty
    <*> (listToMaybeList <$> parseText)

parseFileProperty :: Parser Property
parseFileProperty = do
  traceM "parseFileProperty"
  propertyName <- between (string "#+") (char ':') identifier
  skipMany singleSpace
  propertyValue <- restOfLine
  pure Property {..}

line :: Parser Text
line = pack <$> manyTill (printChar <|> singleSpace) (try trailingSpace)

parseHeaderStars :: Parser Int
parseHeaderStars = length <$> someTill (char '*') (some singleSpace)

listToMaybeList :: [Text] -> Maybe [Text]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

parseText :: Parser [Text]
parseText = do
  traceM "parseText"
  manyTill line (try (void (lookAhead parseHeaderStars)) <|> eof)

data OrgEntry = OrgEntry
  { entryDepth :: Int,
    entryKeyword :: Maybe Text,
    entryPriority :: Maybe Text,
    entryTitle :: Text,
    entryContext :: Maybe Text,
    entryLocation :: Maybe Text,
    entryTags :: [Text],
    entryStamps :: [OrgStamp],
    entryProperties :: [Property],
    entryText :: Maybe [Text]
  }
  deriving (Show)

parseEntry :: Parser OrgEntry
parseEntry = do
  traceM "parseEntry"
  entryDepth <- parseHeaderStars
  OrgConfig {..} <- ask
  entryKeyword <-
    optional $
      ( foldr
          (\x rest -> string x <|> rest)
          mzero
          (openKeywords ++ closedKeywords)
      )
        <* some singleSpace
  entryPriority <- optional parseEntryPriority
  entryContext <- optional parseEntryContext
  (entryTitle, (entryLocation, entryTags)) <-
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
  traceM $
    T.unpack $
      pack (show entryDepth)
        <> " "
        <> pack (show entryKeyword)
        <> " "
        <> pack (show entryPriority)
        <> " \""
        <> entryTitle
        <> "\" "
        <> pack (show entryContext)
        <> " "
        <> pack (show entryLocation)
        <> " "
        <> pack (show entryTags)
        <> " \""
        <> T.intercalate " " (map showOrgStamp entryStamps)
        <> "\""
  pure OrgEntry {..}

parseEntryPriority :: Parser Text
parseEntryPriority = do
  prio <- string "[#" *> (pack . (: []) <$> oneOf ['A' .. 'C']) <* char ']'
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

parseTitleSuffix :: Parser (Maybe Text, [Text])
parseTitleSuffix =
  ((Nothing, []) <$ try (skipManyTill singleSpace newline))
    <|> do
      _ <- some singleSpace
      location <- optional parseLocation
      tags <-
        optional
          ( ( case location of
                Nothing -> pure ()
                Just _ -> void $ singleSpace *> some singleSpace
            )
              *> parseTags
          )
      trailingSpace
      pure (location, join (maybeToList tags))

parseLocation :: Parser Text
parseLocation = between (char '{') (char '}') identifier

parseTags :: Parser [Text]
parseTags = filter (not . T.null) <$> (char ':' *> sepBy1 identifier (char ':'))

data OrgStampKind
  = ClosedStamp
  | ScheduledStamp
  | DeadlineStamp
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgStamp = OrgStamp
  { orgStampKind :: OrgStampKind,
    orgStampTime :: OrgTime
  }
  deriving (Show, Eq, Ord)

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

parseOrgStamps :: Parser [OrgStamp]
parseOrgStamps = sepBy1 parseOrgStamp (char ' ')

showOrgStamp :: OrgStamp -> Text
showOrgStamp OrgStamp {..} =
  T.concat
    [ case orgStampKind of
        ClosedStamp -> "CLOSED: "
        ScheduledStamp -> "SCHEDULED: "
        DeadlineStamp -> "DEADLINE: ",
      showOrgTime orgStampTime
    ]

data OrgTimeKind
  = ActiveTime
  | InactiveTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTime = OrgTime
  { orgTimeKind :: OrgTimeKind,
    orgTimeDay :: Day,
    orgTimeDayEnd :: Maybe Day,
    orgTimeStart :: Maybe DiffTime,
    orgTimeEnd :: Maybe DiffTime,
    orgTimeSuffix :: Maybe OrgTimeSuffix
  }
  deriving (Show, Eq, Ord)

data OrgTimeSpan
  = OrgDaySpan
  | OrgWeekSpan
  | OrgMonthSpan
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTimeSuffixKind
  = OrgTimeRepeat
  | OrgTimeDottedRepeat
  | OrgTimeWithin
  deriving (Show, Eq, Ord, Enum, Bounded)

data OrgTimeSuffix = OrgTimeSuffix
  { orgSuffixKind :: OrgTimeSuffixKind,
    orgSuffixNum :: Int,
    orgSuffixSpan :: OrgTimeSpan
  }
  deriving (Show, Eq, Ord)

orgTimeStartToUTCTime :: OrgTime -> UTCTime
orgTimeStartToUTCTime OrgTime {..} =
  UTCTime orgTimeDay (fromMaybe (secondsToDiffTime 0) orgTimeStart)

orgTimeEndToUTCTime :: OrgTime -> Maybe UTCTime
orgTimeEndToUTCTime OrgTime {..} =
  UTCTime
    <$> orgTimeDayEnd
    <*> Just (fromMaybe (secondsToDiffTime 0) orgTimeEnd)

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
  traceM "parseOrgTimeSingle..1"
  _dow <-
    foldr
      (\x rest -> string x <|> rest)
      mzero
      ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  traceM "parseOrgTimeSingle..2"
  orgTimeStart <- optional $ try $ do
    traceM "parseOrgTimeSingle..2a"
    _ <- char ' '
    traceM "parseOrgTimeSingle..2b"
    hour <- count 2 numberChar
    traceM "parseOrgTimeSingle..2c"
    _ <- char ':'
    traceM "parseOrgTimeSingle..2d"
    minute <- count 2 numberChar
    traceM "parseOrgTimeSingle..2e"
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  traceM "parseOrgTimeSingle..3"
  orgTimeEnd <- optional $ do
    guard $ isJust orgTimeStart
    _ <- char '-'
    hour <- count 2 numberChar
    _ <- char ':'
    minute <- count 2 numberChar
    pure $ secondsToDiffTime (read hour * 60 + read minute)
  traceM "parseOrgTimeSingle..4"
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
  traceM "parseOrgTimeSingle..5"
  _ <- case orgTimeKind of
    ActiveTime -> char '>'
    InactiveTime -> char ']'
  traceM "parseOrgTimeSingle..6"
  pure OrgTime {..}

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

showOrgTimeSingle :: OrgTime -> Text
showOrgTimeSingle OrgTime {..} =
  T.concat $
    [ beg,
      pack (formatTime defaultTimeLocale "%Y-%m-%d %a" orgTimeDay)
    ]
      ++ case orgTimeStart of
        Nothing -> []
        Just start ->
          [ pack
              ( formatTime
                  defaultTimeLocale
                  " %H:%M"
                  (UTCTime orgTimeDay start)
              )
          ]
      ++ case orgTimeEnd of
        Nothing -> []
        Just finish ->
          [ pack
              ( formatTime
                  defaultTimeLocale
                  "-%H:%M"
                  (UTCTime orgTimeDay finish)
              )
          ]
      ++ case orgTimeSuffix of
        Nothing -> []
        Just OrgTimeSuffix {..} ->
          [ " ",
            case orgSuffixKind of
              OrgTimeRepeat -> "+"
              OrgTimeDottedRepeat -> ".+"
              OrgTimeWithin -> "-",
            pack (show orgSuffixNum),
            case orgSuffixSpan of
              OrgDaySpan -> "d"
              OrgWeekSpan -> "w"
              OrgMonthSpan -> "m"
          ]
      ++ [ end
         ]
  where
    (beg, end) = case orgTimeKind of
      ActiveTime -> ("<", ">")
      InactiveTime -> ("[", "]")

showOrgTime :: OrgTime -> Text
showOrgTime ts =
  T.concat $
    [ showOrgTimeSingle ts
    ]
      ++ case orgTimeDayEnd ts of
        Nothing -> []
        Just end ->
          [ "--",
            showOrgTimeSingle
              ts
                { orgTimeDay = end,
                  orgTimeDayEnd = Nothing,
                  orgTimeStart = orgTimeEnd ts,
                  orgTimeEnd = Nothing
                }
          ]
