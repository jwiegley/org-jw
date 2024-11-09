{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.Parse (parseOrgFile, parseTime) where

import Control.Applicative (asum)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum, isPrint, isSpace)
import Data.Maybe (isJust, maybeToList)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
-- import Debug.Trace
import FlatParse.Combinators
import FlatParse.Stateful hiding (Parser)
import FlatParse.Stateful qualified as FP
import Org.Types

type Parser = FP.Parser (FilePath, Config) (Loc, String)

getLoc :: Parser Loc
getLoc = do
  p <- getPos
  (path, _) <- ask
  pure $ Loc path (unPos p)

parseOrgFile :: Config -> FilePath -> ByteString -> Either (Loc, String) OrgFile
parseOrgFile cfg path =
  resultToEither path . runParser parseOrgFile' (path, cfg) 0

parseOrgFile' :: Parser OrgFile
parseOrgFile' = do
  -- traceM "parseOrgFile..1"
  (path, _) <- ask
  -- traceM $ "parseOrgFile..2: " ++ show path
  OrgFile path
    <$> parseHeader
    <*> many (parseEntry 1)
    <* ( eof <|> do
           !loc <- getLoc
           err (loc, "Trailing text in Org file")
       )

parseProperties :: Parser [Property]
parseProperties = do
  -- traceM "parseProperties..1"
  $(string ":PROPERTIES:") *> trailingSpace
  -- RULE: Property blocks are never empty
  -- traceM "parseProperties..2"
  props <- some $ do
    -- traceM "parseProperties..3"
    _propertyLoc <- getLoc
    -- traceM "parseProperties..4"
    _name <- between $(char ':') $(char ':') identifier
    -- traceM "parseProperties..5"
    guard $ _name /= "END"
    -- traceM "parseProperties..6"
    skipMany singleSpace
    -- traceM "parseProperties..7"
    _value <- restOfLine
    -- traceM "parseProperties..8"
    let _inherited = False
    -- traceM "parseProperties..9"
    pure Property {..}
  -- traceM $ "parseProperties..10: props = " ++ show props
  $(string ":END:") *> trailingSpace
  -- traceM "parseProperties..11"
  return props

{-
parseFromProperty :: Parser a -> [Property] -> String -> Parser (Maybe (Loc, a))
parseFromProperty parser props nm = do
  -- traceM "parseFromProperty..1"
  case props ^? lookupProperty' nm of
    Nothing -> pure Nothing
    Just (Property loc _ _ filetags) -> do
      cfg <- ask
      case runParser
        (parser <* eof)
        cfg
        (loc ^. pos)
        (T.encodeUtf8 (T.pack filetags)) of
        Fail -> failed
        Err msg -> err msg
        OK x _ _ -> pure $ Just (loc, x)

stampFromProperty :: (Loc -> Time -> a) -> String -> [Property] -> Parser [a]
stampFromProperty f nm props = do
  -- traceM "stampFromProperty..1"
  maybe [] ((: []) . uncurry f)
    <$> parseFromProperty parseTimeSingle props nm
-}

parseHeader :: Parser Header
parseHeader = do
  -- traceM "parseHeader..1"
  _headerPropertiesDrawer <-
    join . maybeToList <$> optional parseProperties
  _headerFileProperties <- many parseFileProperty
  -- _headerTags <-
  --   maybe [] snd
  --     <$> parseFromProperty parseTags _headerFileProperties "filetags"
  -- _headerStamps <-
  --   (\x y z -> x ++ y ++ z)
  --     <$> stampFromProperty CreatedStamp "created" _headerPropertiesDrawer
  --     <*> stampFromProperty EditedStamp "edited" _headerPropertiesDrawer
  --     <*> stampFromProperty DateStamp "date" _headerFileProperties
  _headerPreamble <- parseEntryBody 1
  pure Header {..}

parseHeaderStars :: Parser Int
parseHeaderStars = do
  -- traceM "parseHeaderStars..1"
  length <$> some $(char '*') <* some singleSpace

parseFileProperty :: Parser Property
parseFileProperty = do
  -- traceM "parseFileProperty..1"
  _propertyLoc <- getLoc
  _name <- between $(string "#+") $(char ':') identifier
  skipMany singleSpace
  _value <- restOfLine
  let _inherited = False
  pure Property {..}

parseKeyword :: Parser Keyword
parseKeyword = do
  -- traceM "parseKeyword..1"
  !loc <- getLoc
  (_, cfg) <- ask
  asum $
    map
      (\kw -> OpenKeyword loc kw <$ byteString (strToUtf8 kw))
      (_openKeywords cfg)
      ++ map
        (\kw -> ClosedKeyword loc kw <$ byteString (strToUtf8 kw))
        (_closedKeywords cfg)

parseEntry :: Int -> Parser Entry
parseEntry parseAtDepth = do
  -- traceM "parseEntry..1"
  _entryLoc <- getLoc
  -- traceM "parseEntry..2"
  _entryDepth <- do
    -- traceM "parseEntry..3"
    depth <- parseHeaderStars
    -- traceM $ "parseEntry..4: " ++ show depth ++ " == " ++ show parseAtDepth
    guard $ depth == parseAtDepth
    -- traceM "parseEntry..5"
    pure depth
  -- traceM "parseEntry..6"
  _entryHeadline <- lookahead restOfLine
  -- In order to support parsing of inline tasks, we do not parse entries
  -- whole whole headline is "END".
  guard $ _entryHeadline /= "END"
  -- traceM "parseEntry..7"
  _entryKeyword <- optional (parseKeyword <* some singleSpace)
  -- traceM "parseEntry..8"
  _entryPriority <- optional parseEntryPriority
  -- traceM "parseEntry..9"
  _entryContext <- optional parseEntryContext
  -- traceM "parseEntry..9.5"
  _entryVerb <- optional parseEntryVerb
  -- traceM "parseEntry..10"
  skipMany singleSpace
  (_entryTitle, (_entryLocator, _entryTags)) <-
    manyTill_ anyChar parseTitleSuffix
  -- traceM "parseEntry..11"
  stamps <-
    join . maybeToList
      <$> optional (parseStamps <* trailingSpace)
  -- traceM "parseEntry..12"
  _entryProperties <-
    join . maybeToList
      <$> optional parseProperties
  -- traceM "parseEntry..13"
  activeStamp <- optional $ do
    !loc <- getLoc
    lookahead $(char '<')
    ActiveStamp loc <$> parseTime <* trailingSpace
  let _entryStamps = stamps ++ maybeToList activeStamp
  -- _entryStamps <-
  --   (\x y -> stamps ++ maybeToList activeStamp ++ x ++ y)
  --     <$> stampFromProperty CreatedStamp "created" _entryProperties
  --     <*> stampFromProperty EditedStamp "edited" _entryProperties
  -- traceM "parseEntry..14"
  logEntries <- many parseLogEntry
  -- traceM "parseEntry..15"
  text <- parseEntryBody parseAtDepth
  -- traceM "parseEntry..16"
  let (_entryLogEntries, _entryBody) =
        let dflt = (logEntries, text)
         in case reverse logEntries of
              [] -> dflt
              l : ls -> case previewIsh _LogBody l of
                Just bdy -> case reverse (_blocks bdy) of
                  (le@(Whitespace _ _) : les) ->
                    case ( _blocks text,
                           reverse (_blocks text)
                         ) of
                      (b : bs, Whitespace _ _ : _) ->
                        ( reverse
                            ( setIsh _LogBody (Body (reverse les)) l : ls
                            ),
                          Body (le : b : bs)
                        )
                      _ -> dflt
                  _ -> dflt
                _ -> dflt
  -- traceM "parseEntry..17"
  _entryItems <- many (parseEntry (succ _entryDepth))
  -- traceM "parseEntry..18"
  pure Entry {..}

parseEntryPriority :: Parser String
parseEntryPriority = do
  -- traceM "parseEntryPriority..1"
  $( switch
       [|
         case _ of
           "[#A]" -> pure "A"
           "[#B]" -> pure "B"
           "[#C]" -> pure "C"
         |]
   )

parseEntryVerb :: Parser String
parseEntryVerb = do
  -- traceM "parseEntryVerb..1"
  verb <- some (satisfy (\c -> isAlpha c))
  $(char ':')
  spaces_
  pure verb

parseEntryContext :: Parser String
parseEntryContext = do
  -- traceM "parseEntryContext..1"
  context <-
    between
      ($(char '('))
      ($(char ')'))
      (some (satisfy (\c -> c /= ')' && (isPrint c || c == ' '))))
  spaces_
  pure context

parseTitleSuffix :: Parser (Maybe String, [Tag])
parseTitleSuffix = do
  -- traceM "parseTitleSuffix..1"
  ((Nothing, []) <$ trailingSpace)
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

parseLocation :: Parser String
parseLocation = do
  -- traceM "parseLocation..1"
  between ($(char '{')) ($(char '}')) identifier

parseTags :: Parser [Tag]
parseTags = do
  -- traceM "parseTags..1"
  colon *> endBy1 parseTag colon
  where
    colon = $(char ':')

parseTag :: FP.Parser r e Tag
parseTag = PlainTag <$> tag
  where
    tag :: FP.Parser r e String
    tag =
      many
        ( satisfy $ \ch ->
            isAlphaNum ch
              || ch `elem` ['-', '_', '=', '/']
        )

parseStamps :: Parser [Stamp]
parseStamps = do
  -- traceM "parseStamps..1"
  sepBy1 parseStamp ($(char ' '))

parseStamp :: Parser Stamp
parseStamp = do
  -- traceM "parseStamp..1"
  !loc <- getLoc
  $( switch
       [|
         case _ of
           "CLOSED: " -> ClosedStamp loc <$> parseTimeSingle
           "SCHEDULED: " -> ScheduledStamp loc <$> parseTimeSingle
           "DEADLINE: " -> DeadlineStamp loc <$> parseTimeSingle
         |]
   )

blendTimes :: Time -> Time -> Time
blendTimes start Time {..} =
  start
    { _timeDayEnd = Just _timeDay,
      _timeEnd = _timeStart
    }

parseTime :: Parser Time
parseTime = do
  -- traceM "parseTime..1"
  start <- parseTimeSingle
  mend <- optional $ $(string "--") *> parseTimeSingle
  case mend of
    Nothing -> pure start
    Just tm@Time {..} -> do
      forM_ _timeDayEnd $ \_ -> do
        !loc <- getLoc
        err (loc, "Invalid ending time (has day end): " ++ show tm)
      forM_ _timeEnd $ \_ -> do
        !loc <- getLoc
        err (loc, "Invalid ending time (has end): " ++ show tm)
      forM_ _timeSuffix $ \_ -> do
        !loc <- getLoc
        err (loc, "Invalid ending time (has suffix): " ++ show tm)
      pure $ blendTimes start tm

parseTimeSingle :: Parser Time
parseTimeSingle = do
  -- traceM "parseTimeSingle..1"
  _timeKind <-
    $( switch
         [|
           case _ of
             "<" -> pure ActiveTime
             "[" -> pure InactiveTime
           |]
     )
  -- traceM "parseTimeSingle..2"
  year <- read <$> count 4 digitChar
  -- traceM "parseTimeSingle..3"
  unless (year >= 1970 && year <= 2200) $ do
    !loc <- getLoc
    err (loc, "Year out of range: " ++ show year)
  _ <- $(char '-')
  -- traceM "parseTimeSingle..4"
  month <- read <$> count 2 digitChar
  unless (month >= 1 && month <= 12) $ do
    !loc <- getLoc
    err (loc, "Month out of range: " ++ show month)
  -- traceM "parseTimeSingle..5"
  _ <- $(char '-')
  -- traceM "parseTimeSingle..6"
  day <- read <$> count 2 digitChar
  unless (day >= 1 && month <= 31) $ do
    !loc <- getLoc
    err (loc, "Day out of range: " ++ show day)
  -- traceM "parseTimeSingle..7"
  _timeDay <- case fromGregorianValid year month day of
    Just d -> pure $ fromInteger $ toModifiedJulianDay d
    Nothing -> do
      !loc <- getLoc
      err
        ( loc,
          "Could not parse gregorian date: "
            ++ show year
            ++ "-"
            ++ show month
            ++ "-"
            ++ show day
        )
  -- traceM "parseTimeSingle..8"
  let _timeDayEnd = Nothing
  -- traceM "parseTimeSingle..9"
  _ <- $(char ' ')
  -- traceM "parseTimeSingle..10"
  _dow <-
    $( switch
         [|
           case _ of
             "Sun" -> pure ("Sun" :: String)
             "Mon" -> pure ("Mon" :: String)
             "Tue" -> pure ("Tue" :: String)
             "Wed" -> pure ("Wed" :: String)
             "Thu" -> pure ("Thu" :: String)
             "Fri" -> pure ("Fri" :: String)
             "Sat" -> pure ("Sat" :: String)
           |]
     )
  -- traceM "parseTimeSingle..11"
  _timeStart <- optional $ do
    _ <- $(char ' ')
    hour <- read <$> count 2 digitChar
    unless (hour >= 0 && hour <= 23) $ do
      !loc <- getLoc
      err (loc, "Hour out of range: " ++ show hour)
    _ <- $(char ':')
    minute <- read <$> count 2 digitChar
    unless (minute >= 0 && minute <= 59) $ do
      !loc <- getLoc
      err (loc, "Minute out of range: " ++ show minute)
    pure $ hour * 60 + minute
  -- traceM "parseTimeSingle..12"
  _timeEnd <- optional $ do
    guard $ isJust _timeStart
    _ <- $(char '-')
    hour <- read <$> count 2 digitChar
    unless (hour >= 0 && hour <= 23) $ do
      !loc <- getLoc
      err (loc, "Hour out of range: " ++ show hour)
    _ <- $(char ':')
    minute <- read <$> count 2 digitChar
    unless (minute >= 0 && minute <= 59) $ do
      !loc <- getLoc
      err (loc, "Minute out of range: " ++ show minute)
    pure $ hour * 60 + minute
  -- traceM "parseTimeSingle..13"
  _timeSuffix <- optional $ do
    _ <- $(char ' ')
    repeatDotted <- isJust <$> optional ($(char '.'))
    _suffixKind <-
      $( switch
           [|
             case _ of
               "+" ->
                 pure $
                   if repeatDotted
                     then TimeDottedRepeat
                     else TimeRepeat
               "-" -> pure TimeWithin
             |]
       )
    _suffixNum <- read <$> some digitChar
    unless (_suffixNum >= -100 && _suffixNum < 100) $ do
      !loc <- getLoc
      err (loc, "Time suffix out of range: " ++ show _suffixNum)
    _suffixSpan <-
      $( switch
           [|
             case _ of
               "m" -> pure MonthSpan
               "d" -> pure DaySpan
               "w" -> pure WeekSpan
             |]
       )
    _suffixLargerSpan <- optional $ do
      _ <- $(char '/')
      num <- read <$> some digitChar
      s <-
        $( switch
             [|
               case _ of
                 "m" -> pure MonthSpan
                 "d" -> pure DaySpan
                 "w" -> pure WeekSpan
               |]
         )
      pure (num, s)
    pure TimeSuffix {..}
  -- traceM "parseTimeSingle..14"
  _ <- case _timeKind of
    ActiveTime -> $(char '>')
    InactiveTime -> $(char ']')
  -- traceM "parseTimeSingle..15"
  pure Time {..}

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  -- traceM "parseLogEntry..1"
  !loc <- getLoc
  $( switch
       [|
         case _ of
           "- CLOSING NOTE" ->
             parseClosing loc >>= (<$> trailingNote)
           "- State \"" ->
             parseState loc >>= (<$> trailingNote)
           "- Note taken on" ->
             parseNote loc >>= (<$> trailingNote)
           "- Rescheduled from \"" ->
             parseRescheduled loc >>= (<$> trailingNote)
           "- Not scheduled, was \"" ->
             parseNotScheduled loc >>= (<$> trailingNote)
           "- New deadline from \"" ->
             parseDeadline loc >>= (<$> trailingNote)
           "- Removed deadline, was \"" ->
             parseNoDeadline loc >>= (<$> trailingNote)
           "- Refiled on" ->
             parseRefiling loc >>= (<$> trailingNote)
           "CLOCK:" ->
             parseClockEntry loc
           ":LOGBOOK:" ->
             parseLogBook loc
         |]
   )
  where
    trailingNote = do
      -- traceM "trailingNote..1"
      ( Just
          <$> ( spaces_
                  *> $(string "\\\\")
                  *> trailingSpace
                  *> parseNoteBody
              )
        )
        <|> ( do
                -- traceM "trailingNote..2"
                Nothing <$ trailingSpace
            )

    parseClockEntry loc = do
      -- traceM "parseClockEntry..1"
      _ <- spaces_
      -- traceM "parseClockEntry..2"
      start <- parseTimeSingle
      -- traceM "parseClockEntry..3"
      mend <- optional $ do
        -- traceM "parseClockEntry..4"
        $(string "--")
        -- traceM "parseClockEntry..5"
        end <- parseTimeSingle
        -- traceM "parseClockEntry..6"
        let tm = blendTimes start end
        -- traceM "parseClockEntry..7"
        spaces_ *> $(string "=>") *> spaces_
        -- traceM "parseClockEntry..8"
        -- traceM "parseClockEntry..9"
        _hours <- read <$> some digitChar
        -- traceM "parseClockEntry..10"
        _ <- $(char ':')
        -- traceM "parseClockEntry..11"
        _mins <- read <$> some digitChar
        -- traceM "parseClockEntry..12"
        trailingSpace
        -- traceM "parseClockEntry..13"
        pure (tm, Just (Duration {..}))
      case mend of
        Nothing -> trailingSpace
        _ -> pure ()
      -- traceM "parseClockEntry..14"
      pure $
        maybe
          (LogClock loc start Nothing)
          (uncurry (LogClock loc))
          mend

    parseLogBook loc = do
      -- traceM "parseLogBook..1"
      trailingSpace
      -- traceM "parseLogBook..2"
      book <- many parseLogEntry
      -- traceM $ "parseLogBook..3: " ++ show book
      $(string ":END:") *> trailingSpace
      -- traceM "parseLogBook..4"
      return $ LogBook loc book

    parseClosing loc = do
      -- traceM "parseClosing..1"
      spaces_
      LogClosing loc <$> parseTimeSingle

    parseState loc = do
      -- traceM "parseState..1"
      fromKeyword <- parseKeyword <* $(char '"') <* spaces_
      -- traceM "parseState..2"
      toKeyword <-
        optional $
          $(string "from")
            *> spaces_
            *> $(char '"')
            *> parseKeyword
            <* $(char '"')
            <* spaces_
      -- traceM "parseState..3"
      LogState loc fromKeyword toKeyword <$> parseTimeSingle

    parseNote loc = do
      -- traceM "parseNote..1"
      spaces_
      LogNote loc <$> parseTimeSingle

    parseRescheduled loc = do
      -- traceM "parseRescheduled..1"
      origTime <- parseTimeSingle
      $(string "\" on") <* spaces_
      LogRescheduled loc origTime <$> parseTimeSingle

    parseNotScheduled loc = do
      -- traceM "parseNotScheduled..1"
      origTime <- parseTimeSingle
      $(string "\" on") <* spaces_
      LogNotScheduled loc origTime <$> parseTimeSingle

    parseDeadline loc = do
      -- traceM "parseDeadline..1"
      origTime <- parseTimeSingle
      $(string "\" on") <* spaces_
      LogDeadline loc origTime <$> parseTimeSingle

    parseNoDeadline loc = do
      -- traceM "parseNoDeadline..1"
      origTime <- parseTimeSingle
      $(string "\" on") <* spaces_
      LogNoDeadline loc origTime <$> parseTimeSingle

    parseRefiling loc = do
      -- traceM "parseRefiling..1"
      spaces_
      LogRefiling loc <$> parseTimeSingle

parseEntryBody :: Int -> Parser Body
parseEntryBody parseAtDepth = do
  -- traceM "parseEntryBody..1"
  parseBody (pure ()) $
    -- This special logic is needed to support parsing of inline tasks, since
    -- they end not in another entry, but a special marker of 15 stars
    -- followed by the word "END".
    if parseAtDepth < 15
      then do
        depth <- parseHeaderStars
        -- traceM $ "parseEntryBody..2: " ++ show depth
        guard $ depth < 15
      else do
        -- depth <- parseHeaderStars
        -- traceM $ "parseEntryBody..3: " ++ show depth
        void parseHeaderStars

parseNoteBody :: Parser Body
parseNoteBody = do
  -- traceM "parseNoteBody..1"
  parseBody
    ($(string "  ") <|> lookahead newline)
    (satisfy (not . isSpace))

parseBody :: Parser a -> Parser b -> Parser Body
parseBody leader terminus = do
  -- traceM "parseBody..1"
  mconcat
    <$> manyTill
      (Body . (: []) <$> parseBlock leader)
      (lookahead (void terminus) <|> eof)

parseBlock :: Parser a -> Parser Block
parseBlock leader = do
  -- traceM "parseBlock..1"
  !loc <- getLoc
  parseWhitespaceBlock loc
    <|> parseDrawerBlock loc
    <|> parseInlineTask loc
    <|> parseParagraphBlock loc
  where
    parseWhitespaceBlock loc = do
      -- traceM "parseWhitespaceBlock..1"
      Whitespace loc <$> manyTill (satisfy (== ' ')) newline

    parseDrawerBlock loc = do
      -- traceM "parseDrawerBlock..1"
      uncurry (Drawer loc) <$> (leader *> parseDrawer leader)

    parseInlineTask loc = do
      -- traceM "parseInlineTask..1"
      InlineTask loc
        <$> try
          ( parseEntry 15 <* do
              -- traceM "parseInlineTask..2"
              depth <- parseHeaderStars
              -- traceM $ "parseInlineTask..3: " ++ show depth
              guard $ depth == 15
              -- traceM "parseInlineTask..4"
              $(string "END")
              -- traceM "parseInlineTask..5"
              _ <- restOfLine
              -- traceM "parseInlineTask..6"
              pure ()
          )

    parseParagraphBlock loc = do
      -- traceM "parseParagraphBlock..1"
      Paragraph loc . (: []) <$> (leader *> lineOrEof)

parseDrawer :: Parser a -> Parser (DrawerType, [String])
parseDrawer leader = do
  -- traceM "parseDrawer..1"
  prefix <- many (satisfy (== ' '))
  $( switch
       [|
         case _ of
           ":" -> parsePlainDrawer prefix
           "#+begin" -> parseSrcDrawer prefix "#+begin"
           "#+BEGIN" -> parseSrcDrawer prefix "#+BEGIN"
         |]
   )
  where
    endDrawerString =
      $( switch
           [|
             case _ of
               ":end:" -> pure ":end:"
               ":END:" -> pure ":END:"
             |]
       )

    parsePlainDrawer prefix = do
      -- traceM "parsePlainDrawer..1"
      txt <- do
        -- traceM "parsePlainDrawer..2"
        -- jww (2024-09-11): Draw name should be included as:
        -- @Drawer Loc String [String]@
        ident <- identifier <* $(char ':') <* trailingSpace
        -- traceM "parsePlainDrawer..3"
        pure $ ":" <> ident <> ":"
      -- traceM "parsePlainDrawer..4"
      content <-
        manyTill
          (("\n" <$ newline) <|> (leader *> restOfLine))
          ( lookahead
              ( void
                  ( leader
                      *> byteString (T.encodeUtf8 (T.pack prefix))
                      *> endDrawerString
                  )
                  <|> eof
              )
          )
      -- traceM "parsePlainDrawer..5"
      endLine <- do
        -- traceM "parsePlainDrawer..6"
        _ <- leader *> byteString (T.encodeUtf8 (T.pack prefix))
        -- traceM "parsePlainDrawer..7"
        ending <- endDrawerString <* trailingSpace
        -- traceM "parsePlainDrawer..8"
        pure $ prefix <> ending
      -- traceM "parsePlainDrawer..9"
      pure (PlainDrawer txt, prefix <> txt : content ++ [endLine])

    endBlockString =
      $( switch
           [|
             case _ of
               "#+end" -> pure "#+end"
               "#+END" -> pure "#+END"
             |]
       )

    parseSrcDrawer prefix begin = do
      -- traceM "parseSrcDrawer..1"
      txt <- do
        suffix <- wholeLine
        pure $ begin <> suffix
      content <-
        manyTill
          (("" <$ newline) <|> (leader *> restOfLine))
          ( lookahead
              ( void
                  ( leader
                      *> byteString (T.encodeUtf8 (T.pack prefix))
                      *> endBlockString
                  )
                  <|> eof
              )
          )
      endLine <- do
        _ <- leader *> byteString (T.encodeUtf8 (T.pack prefix))
        ending <- endBlockString
        suffix <- wholeLine
        pure $ prefix <> ending <> suffix
      pure
        ( BeginDrawer (unwords (Prelude.take 2 (words txt))),
          prefix <> txt : content ++ [endLine]
        )
