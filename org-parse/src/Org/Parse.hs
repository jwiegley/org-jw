{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Org.Parse where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isPrint, isSpace)
import Data.Maybe (isJust, maybeToList)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import FlatParse.Combinators
import FlatParse.Stateful hiding (Parser)
import FlatParse.Stateful qualified as FP
import Org.Types

-- import Debug.Trace

type Parser = FP.Parser (FilePath, Config) String

getLoc :: Parser Loc
getLoc = do
  p <- getPos
  (path, _) <- ask
  pure $ Loc path (unPos p)

readOrgFile_ :: Config -> FilePath -> ByteString -> Result String OrgFile
readOrgFile_ cfg path = runParser parseOrgFile (path, cfg) 0

parseOrgFile :: Parser OrgFile
parseOrgFile = do
  -- traceM "parseOrgFile..1"
  (path, _) <- ask
  -- traceM $ "parseOrgFile..2: " ++ show path
  OrgFile path <$> parseHeader <*> many (parseEntry 1) <* eof

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
  _headerPreamble <- parseEntryBody
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
  loc <- getLoc
  $( switch
       [|
         case _ of
           "TODO" -> pure $ OpenKeyword loc "TODO"
           "PROJECT" -> pure $ OpenKeyword loc "PROJECT"
           "DOING" -> pure $ OpenKeyword loc "DOING"
           "WAIT" -> pure $ OpenKeyword loc "WAIT"
           "DEFER" -> pure $ OpenKeyword loc "DEFER"
           "DELEGATED" -> pure $ OpenKeyword loc "DELEGATED"
           "APPT" -> pure $ OpenKeyword loc "APPT"
           "DONE" -> pure $ ClosedKeyword loc "DONE"
           "CANCELED" -> pure $ ClosedKeyword loc "CANCELED"
           "NOTE" -> pure $ ClosedKeyword loc "NOTE"
           "LINK" -> pure $ ClosedKeyword loc "LINK"
         |]
   )

parseEntry :: Int -> Parser Entry
parseEntry parseAtDepth = do
  -- traceM "parseEntry..1"
  _entryLoc <- getLoc
  _entryDepth <- do
    depth <- parseHeaderStars
    guard $ depth == parseAtDepth
    pure depth
  _entryHeadline <- lookahead restOfLine
  _entryKeyword <- optional (parseKeyword <* some singleSpace)
  _entryPriority <- optional parseEntryPriority
  _entryContext <- optional parseEntryContext
  (_entryTitle, (_entryLocator, _entryTags)) <-
    manyTill_ anyChar parseTitleSuffix
  stamps <-
    join . maybeToList
      <$> optional (parseStamps <* trailingSpace)
  _entryProperties <-
    join . maybeToList
      <$> optional parseProperties
  activeStamp <- optional $ do
    loc <- getLoc
    lookahead $(char '<')
    ActiveStamp loc <$> parseTime <* trailingSpace
  let _entryStamps = stamps ++ maybeToList activeStamp
  -- _entryStamps <-
  --   (\x y -> stamps ++ maybeToList activeStamp ++ x ++ y)
  --     <$> stampFromProperty CreatedStamp "created" _entryProperties
  --     <*> stampFromProperty EditedStamp "edited" _entryProperties
  logEntries <- many parseLogEntry
  text <- parseEntryBody
  let (_entryLogEntries, _entryString) =
        let dflt = (logEntries, text)
         in case reverse logEntries of
              [] -> dflt
              l : ls -> case l ^? _LogBody of
                Nothing -> dflt
                Just bdy -> case reverse (_blocks bdy) of
                  (le@(Whitespace _ _) : les) ->
                    case ( _blocks text,
                           reverse (_blocks text)
                         ) of
                      (b : bs, Whitespace _ _ : _) ->
                        ( reverse ((l & _LogBody .~ Body (reverse les)) : ls),
                          Body (le : b : bs)
                        )
                      _ -> dflt
                  _ -> dflt
  _entryItems <- many (parseEntry (succ _entryDepth))
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

parseTag :: FP.Parser r String Tag
parseTag = PlainTag <$> tag
  where
    tag :: FP.Parser r String String
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
  loc <- getLoc
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

parseTime :: FP.Parser r String Time
parseTime = do
  -- traceM "parseTime..1"
  start <- parseTimeSingle
  mend <- optional $ $(string "--") *> parseTimeSingle
  case mend of
    Nothing -> pure start
    Just tm@Time {..} -> do
      forM_ _timeDayEnd $ \_ ->
        err $ "Invalid ending time (has day end): " ++ show tm
      forM_ _timeEnd $ \_ ->
        err $ "Invalid ending time (has end): " ++ show tm
      forM_ _timeSuffix $ \_ ->
        err $ "Invalid ending time (has suffix): " ++ show tm
      pure $ blendTimes start tm

parseTimeSingle :: FP.Parser r String Time
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
  year <- count 4 digitChar
  -- traceM "parseTimeSingle..3"
  _ <- $(char '-')
  -- traceM "parseTimeSingle..4"
  month <- count 2 digitChar
  -- traceM "parseTimeSingle..5"
  _ <- $(char '-')
  -- traceM "parseTimeSingle..6"
  day <- count 2 digitChar
  -- traceM "parseTimeSingle..7"
  _timeDay <- case fromGregorianValid (read year) (read month) (read day) of
    Just d -> pure $ fromInteger $ toModifiedJulianDay d
    Nothing ->
      err $
        "Could not parse gregorian date: "
          ++ year
          ++ "-"
          ++ month
          ++ "-"
          ++ day
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
    hour <- count 2 digitChar
    _ <- $(char ':')
    minute <- count 2 digitChar
    pure $ read hour * 60 + read minute
  -- traceM "parseTimeSingle..12"
  _timeEnd <- optional $ do
    guard $ isJust _timeStart
    _ <- $(char '-')
    hour <- count 2 digitChar
    _ <- $(char ':')
    minute <- count 2 digitChar
    pure $ read hour * 60 + read minute
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
  loc <- getLoc
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
        pure (tm, Duration {..})
      -- traceM "parseClockEntry..14"
      pure $
        maybe
          (LogClock loc start Nothing)
          (uncurry (LogClock loc) . second Just)
          mend

    parseLogBook loc = do
      -- traceM "parseLogBook..1"
      trailingSpace
      -- traceM "parseLogBook..2"
      book <- many parseLogEntry
      -- traceM "parseLogBook..3"
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

parseEntryBody :: Parser Body
parseEntryBody = do
  -- traceM "parseEntryBody..1"
  parseBody (pure ()) parseHeaderStars

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
  loc <- getLoc
  parseWhitespaceBlock loc
    <|> parseDrawerBlock loc
    <|> parseParagraphBlock loc
  where
    parseWhitespaceBlock loc = do
      -- traceM "parseWhitespaceBlock..1"
      Whitespace loc <$> manyTill (satisfy (== ' ')) newline

    parseDrawerBlock loc = do
      -- traceM "parseDrawerBlock..1"
      Drawer loc <$> (leader *> parseDrawer leader)

    parseParagraphBlock loc = do
      -- traceM "parseParagraphBlock..1"
      Paragraph loc . (: []) <$> (leader *> lineOrEof)

parseDrawer :: Parser a -> Parser [String]
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
        ident <- identifier <* $(char ':') <* trailingSpace
        -- traceM "parsePlainDrawer..3"
        pure $ prefix <> ":" <> ident <> ":"
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
      pure $ txt : content ++ [endLine]

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
        pure $ prefix <> begin <> suffix
      content <-
        manyTill
          (("\n" <$ newline) <|> (leader *> restOfLine))
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
      pure $ txt : content ++ [endLine]
