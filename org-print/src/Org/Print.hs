{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Print
  ( showOrgFile,
    showTime,
    showBlock,
    showEntry,
    summarizeEntry,
  )
where

import Control.Applicative (asum)
import Control.Monad.Reader
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isNothing, maybeToList)
import Data.Time
import Org.Types
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

showStamp :: Stamp -> String
showStamp (ClosedStamp _ tm) = "CLOSED: " <> showTime tm
showStamp (ScheduledStamp _ tm) = "SCHEDULED: " <> showTime tm
showStamp (DeadlineStamp _ tm) = "DEADLINE: " <> showTime tm
showStamp x = error $ "showStamp not support for " ++ show x

showTime :: Time -> String
showTime = showTime' False

showTime' :: Bool -> Time -> String
showTime' splitTime tm =
  concat $
    showTimeSingle' splitTime tm
      : case _timeDayEnd tm of
        Just end
          | splitTime || end /= _timeDay tm ->
              [ "--",
                showTimeSingle
                  tm
                    { _timeDay = end,
                      _timeDayEnd = Nothing,
                      _timeStart = _timeEnd tm,
                      _timeEnd = Nothing
                    }
              ]
        _ -> []

showTimeSingle :: Time -> String
showTimeSingle = showTimeSingle' False

showTimeSingle' :: Bool -> Time -> String
showTimeSingle' splitTime Time {..} =
  concat $
    [ beg,
      formatTime
        defaultTimeLocale
        "%Y-%m-%d %a"
        (ModifiedJulianDay _timeDay)
    ]
      ++ case _timeStart of
        Nothing -> []
        Just start ->
          [ formatTime
              defaultTimeLocale
              " %H:%M"
              ( UTCTime
                  (ModifiedJulianDay _timeDay)
                  (secondsToDiffTime (start * 60))
              )
          ]
      ++ case _timeEnd of
        Just finish
          | not splitTime
              && ( isNothing _timeDayEnd
                     || _timeDayEnd == Just _timeDay
                 ) ->
              [ formatTime
                  defaultTimeLocale
                  "-%H:%M"
                  ( UTCTime
                      (ModifiedJulianDay _timeDay)
                      (secondsToDiffTime (finish * 60))
                  )
              ]
        _ -> []
      ++ case _timeSuffix of
        Nothing -> []
        Just TimeSuffix {..} ->
          [ " ",
            case _suffixKind of
              TimeRepeat -> "+"
              TimeRepeatPlus -> "++"
              TimeDottedRepeat -> ".+"
              TimeWithin -> "-",
            show _suffixNum,
            case _suffixSpan of
              DaySpan -> "d"
              WeekSpan -> "w"
              MonthSpan -> "m"
              YearSpan -> "y"
          ]
            ++ case _suffixLargerSpan of
              Nothing -> []
              Just (num, s) ->
                [ "/" <> show num,
                  case s of
                    DaySpan -> "d"
                    WeekSpan -> "w"
                    MonthSpan -> "m"
                    YearSpan -> "y"
                ]
      ++ [ end
         ]
  where
    (beg, end) = case _timeKind of
      ActiveTime -> ("<", ">")
      InactiveTime -> ("[", "]")

showDuration :: Duration -> String
showDuration Duration {..} =
  pad ' ' (show _hours) <> ":" <> pad '0' (show _mins)
  where
    pad c [x] = [c, x]
    pad _ xs = xs

showLogEntry :: LogEntry -> Reader Config [String]
showLogEntry (LogClosing _ tm text) =
  ( ( "- CLOSING NOTE"
        <> showTime tm
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogState _ fr t tm text) =
  ( concat
      ( ["- State "]
          ++ [padded 13 ("\"" <> showKeyword fr <> "\"")]
          ++ [ padded 18 ("from \"" <> showKeyword k <> "\"")
               | k <- maybeToList t
             ]
          ++ [ showTime tm,
               if null text then "" else " \\\\"
             ]
      )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
  where
    padded n s = s <> replicate (n - length s) ' '
showLogEntry (LogNote _ tm text) =
  ( ( "- Note taken on "
        <> showTime tm
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogRescheduled _ tm1 tm2 text) =
  ( ( "- Rescheduled from \""
        <> showTime tm1
        <> "\" on "
        <> showTime tm2
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogNotScheduled _ tm1 tm2 text) =
  ( ( "- Not scheduled, was \""
        <> showTime tm1
        <> "\" on "
        <> showTime tm2
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogDeadline _ tm1 tm2 text) =
  ( ( "- New deadline from \""
        <> showTime tm1
        <> "\" on "
        <> showTime tm2
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogNoDeadline _ tm1 tm2 text) =
  ( ( "- Removed deadline, was \""
        <> showTime tm1
        <> "\" on "
        <> showTime tm2
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogRefiling _ tm text) =
  ( ( "- Refiled on "
        <> showTime tm
        <> if null text then "" else " \\\\"
    )
      :
  )
    <$> maybe (pure []) (showBody "  ") text
showLogEntry (LogClock _ tm Nothing) =
  pure ["CLOCK: " <> showTimeSingle tm]
showLogEntry (LogClock _ tm (Just dur)) =
  pure ["CLOCK: " <> showTime' True tm <> " => " <> showDuration dur]
showLogEntry (LogBook _ tms) = do
  entries <- concatMapM showLogEntry tms
  pure $ ":LOGBOOK:" : entries ++ [":END:"]

showKeyword :: Keyword -> String
showKeyword (OpenKeyword _ n) = n
showKeyword (ClosedKeyword _ n) = n

showEntry :: Entry -> Reader Config [String]
showEntry Entry {..} = do
  props <- properties
  logEnts <- logEntries
  entry <- entryLines
  items <- concatMapM showEntry _entryItems
  tagsCol <- asks _tagsColumn
  pure $
    [title tagsCol]
      ++ timestamps
      ++ props
      ++ logEnts
      ++ activeStamp
      ++ entry
      ++ items
  where
    title tagsCol
      | null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        count x = length . filter (x ==)

        prefixLength =
          -(count '=' prefix)
            + case prefix =~ ("\\[\\[[^]]+\\]\\[" :: String) of
              AllTextSubmatches ([link] :: [String]) ->
                length prefix - length link - 2
              _ -> length prefix

        spacer
          | width < 2 = "  "
          | otherwise = replicate (fromIntegral width) ' '
          where
            width =
              tagsCol
                - fromIntegral prefixLength
                - fromIntegral (length suffix)
        prefix =
          concat $
            [replicate (fromIntegral _entryDepth) '*']
              ++ [" "]
              ++ [showKeyword kw <> " " | kw <- maybeToList _entryKeyword]
              ++ ["[#" <> prio <> "] " | prio <- maybeToList _entryPriority]
              ++ [ "(" <> c <> ") "
                   | c <- maybeToList _entryContext
                 ]
              ++ [ c <> ": "
                   | c <- maybeToList _entryVerb
                 ]
              ++ [_entryTitle]
              ++ [ " {" <> c <> "}"
                   | c <- maybeToList _entryLocator
                 ]
        suffix
          | not (null _entryTags) =
              concat $
                ":"
                  : [ case tag of
                        PlainTag t -> t <> ":"
                      | tag <- _entryTags
                    ]
          | otherwise = ""
    timestamps
      | null leadingStamps = []
      | otherwise = [unwords (map showStamp leadingStamps)]
      where
        leadingStamps = filter isLeadingStamp _entryStamps
    properties
      | null _entryProperties = pure []
      | otherwise = showProperties _entryProperties
    logEntries = concatMapM showLogEntry _entryLogEntries
    activeStamp = case asum
      ( map
          ( \case
              ActiveStamp _ t -> Just t
              _ -> Nothing
          )
          _entryStamps
      ) of
      Nothing -> []
      Just stamp -> [showTime stamp]
    entryLines = showBody "" _entryBody

showBody :: String -> Body -> Reader Config [String]
showBody leader (Body b) = concatMapM (showBlock leader) b

prefixLeader :: String -> String -> String
prefixLeader _ "" = ""
prefixLeader leader str = leader <> str

showBlock :: String -> Block -> Reader Config [String]
showBlock _ (Whitespace _ txt) = pure [txt]
showBlock leader (Paragraph _ xs) = pure $ map (prefixLeader leader) xs
showBlock leader (Drawer _ _ xs) = pure $ map (prefixLeader leader) xs
showBlock _ (InlineTask _ e) = do
  entry <- showEntry e
  pure $ entry ++ ["*************** END"]

bodyLength :: Body -> Reader Config Int
bodyLength body = do
  txt <- showBody "" body
  pure $ sum $ Prelude.map (fromIntegral . length) txt

showOrgFile :: Config -> OrgFile -> [String]
showOrgFile cfg OrgFile {..} =
  flip runReader cfg $
    (++)
      <$> showHeader _orgFileHeader
      <*> concatMapM showEntry _orgFileEntries

showHeader :: Header -> Reader Config [String]
showHeader Header {..} = do
  propDrawer <- propertiesDrawer
  fileProps <- fileProperties
  preamb <- preamble
  pure $ propDrawer ++ fileProps ++ preamb
  where
    propertiesDrawer
      | null _headerPropertiesDrawer = pure []
      | otherwise = showProperties _headerPropertiesDrawer
    fileProperties
      | null _headerFileProperties = pure []
      | otherwise = pure $ showFileProperties _headerFileProperties
    preamble = showBody "" _headerPreamble

showProperties :: [Property] -> Reader Config [String]
showProperties ps = ReaderT $ \cfg ->
  Identity $
    [":PROPERTIES:"]
      ++ map (propLine (_propertyColumn cfg)) ps
      ++ [":END:"]
  where
    propLine propCol Property {..}
      | null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 1 = " "
          | otherwise = replicate (fromIntegral width) ' '
          where
            width = propCol - fromIntegral (length prefix)
        prefix = ":" <> _name <> ":"
        suffix = _value

showFileProperties :: [Property] -> [String]
showFileProperties ps =
  [ "#+" <> _name <> ": " <> _value
    | Property {..} <- ps
  ]

summarizeEntry :: Config -> Entry -> [String]
summarizeEntry cfg Entry {..} =
  [replicate (fromIntegral _entryDepth) '*' <> " " <> _entryTitle]
    ++ runReader
      ( do
          bodyLen <- bodyLength _entryBody
          showProperties
            ( _entryProperties
                ++ [Property _entryLoc False "FILE" (_file _entryLoc)]
                ++ [Property _entryLoc False "OFFSET" (show (_pos _entryLoc))]
                ++ [ Property _entryLoc False "KEYWORD" (show x)
                     | x <- maybeToList _entryKeyword
                   ]
                ++ [ Property _entryLoc False "PRIORITY" x
                     | x <- maybeToList _entryPriority
                   ]
                ++ [ Property _entryLoc False "CONTEXT" x
                     | x <- maybeToList _entryContext
                   ]
                ++ [ Property _entryLoc False "VERB" x
                     | x <- maybeToList _entryVerb
                   ]
                ++ [ Property _entryLoc False "LOCATOR" x
                     | x <- maybeToList _entryLocator
                   ]
                ++ [ Property
                       _entryLoc
                       False
                       "LOG_ENTRIES"
                       (show (length _entryLogEntries))
                     | not (null _entryLogEntries)
                   ]
                ++ [ Property
                       _entryLoc
                       False
                       "BODY_LEN"
                       (show bodyLen)
                     | not (emptyBody _entryBody)
                   ]
                ++ case _entryTags of
                  [] -> []
                  _ ->
                    [ Property
                        _entryLoc
                        False
                        "TAGS"
                        ( concat $
                            ":"
                              : [ case tag of
                                    PlainTag t -> t <> ":"
                                  | tag <- _entryTags
                                ]
                        )
                    ]
                ++ map
                  ( \case
                      ClosedStamp _ tm ->
                        Property _entryLoc False "CLOSED" (showTime tm)
                      ScheduledStamp _ tm ->
                        Property _entryLoc False "SCHEDULED" (showTime tm)
                      DeadlineStamp _ tm ->
                        Property _entryLoc False "DEADLINE" (showTime tm)
                      ActiveStamp _ tm ->
                        Property _entryLoc False "ACTIVE" (showTime tm)
                  )
                  _entryStamps
            )
      )
      cfg {_propertyColumn = 0}
    ++ concatMap (summarizeEntry cfg) _entryItems
