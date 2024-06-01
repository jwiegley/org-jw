{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Printer where

import Control.Lens
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Time
import Org.Types hiding (propertyColumn, tagsColumn)

showStamp :: Stamp -> Text
showStamp (ClosedStamp _ tm) = "CLOSED: " <> showTime tm
showStamp (ScheduledStamp _ tm) = "SCHEDULED: " <> showTime tm
showStamp (DeadlineStamp _ tm) = "DEADLINE: " <> showTime tm
showStamp x = error $ "showStamp not support for " ++ show x

showTime :: Time -> Text
showTime tm =
  T.concat $
    showTimeSingle tm
      : case _timeDayEnd tm of
        Nothing -> []
        Just end ->
          [ "--",
            showTimeSingle
              tm
                { _timeDay = end,
                  _timeDayEnd = Nothing,
                  _timeStart = _timeEnd tm,
                  _timeEnd = Nothing
                }
          ]

showTimeSingle :: Time -> Text
showTimeSingle Time {..} =
  T.concat $
    [ beg,
      pack
        ( formatTime
            defaultTimeLocale
            "%Y-%m-%d %a"
            (ModifiedJulianDay _timeDay)
        )
    ]
      ++ case _timeStart of
        Nothing -> []
        Just start ->
          [ pack
              ( formatTime
                  defaultTimeLocale
                  " %H:%M"
                  ( UTCTime
                      (ModifiedJulianDay _timeDay)
                      (secondsToDiffTime (start * 60))
                  )
              )
          ]
      ++ case _timeEnd of
        Nothing -> []
        Just finish ->
          [ pack
              ( formatTime
                  defaultTimeLocale
                  "-%H:%M"
                  ( UTCTime
                      (ModifiedJulianDay _timeDay)
                      (secondsToDiffTime (finish * 60))
                  )
              )
          ]
      ++ case _timeSuffix of
        Nothing -> []
        Just TimeSuffix {..} ->
          [ " ",
            case _suffixKind of
              TimeRepeat -> "+"
              TimeDottedRepeat -> ".+"
              TimeWithin -> "-",
            pack (show _suffixNum),
            case _suffixSpan of
              DaySpan -> "d"
              WeekSpan -> "w"
              MonthSpan -> "m"
          ]
            ++ case _suffixLargerSpan of
              Nothing -> []
              Just (num, s) ->
                [ pack (show num),
                  case s of
                    DaySpan -> "d"
                    WeekSpan -> "w"
                    MonthSpan -> "m"
                ]
      ++ [ end
         ]
  where
    (beg, end) = case _timeKind of
      ActiveTime -> ("<", ">")
      InactiveTime -> ("[", "]")

showDuration :: Duration -> Text
showDuration Duration {..} =
  T.pack (pad ' ' (show _hours)) <> T.pack (pad '0' (show _mins))
  where
    pad c [x] = [c, x]
    pad _ xs = xs

showLogEntry :: LogEntry -> [Text]
showLogEntry (LogClosing _ tm text) =
  ( "- CLOSING NOTE"
      <> showTime tm
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogState _ fr t tm text) =
  T.concat
    ( [ "- State \"",
        showKeyword fr
      ]
        ++ [ "\" from \"" <> showKeyword k
             | k <- maybeToList t
           ]
        ++ [ "\" ",
             showTime tm,
             if null text then "" else " \\\\"
           ]
    )
    : maybe [] (showBody "  ") text
showLogEntry (LogNote _ tm text) =
  ( "- Note taken on "
      <> showTime tm
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogRescheduled _ tm1 tm2 text) =
  ( "- Rescheduled from \""
      <> showTime tm1
      <> "\" on "
      <> showTime tm2
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogNotScheduled _ tm1 tm2 text) =
  ( "- Not scheduled, was \""
      <> showTime tm1
      <> "\" on "
      <> showTime tm2
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogDeadline _ tm1 tm2 text) =
  ( "- New deadline from \""
      <> showTime tm1
      <> "\" on "
      <> showTime tm2
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogNoDeadline _ tm1 tm2 text) =
  ( "- Removed deadline, was \""
      <> showTime tm1
      <> "\" on "
      <> showTime tm2
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogRefiling _ tm text) =
  ( "- Refiled on "
      <> showTime tm
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogClock _ tm Nothing) =
  ["CLOCK: " <> showTimeSingle tm]
showLogEntry (LogClock _ tm (Just dur)) =
  ["CLOCK: " <> showTime tm <> " => " <> showDuration dur]
showLogEntry (LogBook _ tms) =
  ":LOGBOOK:" : concatMap showLogEntry tms ++ [":END:"]

showKeyword :: Keyword -> Text
showKeyword (OpenKeyword _ n) = n
showKeyword (ClosedKeyword _ n) = n

showEntry :: Int -> Int -> Entry -> [Text]
showEntry propertyColumn tagsColumn Entry {..} =
  [title]
    ++ timestamps
    ++ properties
    ++ logEntries
    ++ activeStamp
    ++ entryLines
    ++ concatMap (showEntry propertyColumn tagsColumn) _entryItems
  where
    title
      | T.null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 2 = "  "
          | otherwise = T.replicate (fromIntegral width) " "
          where
            width =
              tagsColumn
                - fromIntegral (T.length prefix)
                - fromIntegral (T.length suffix)
        prefix =
          T.concat $
            [T.replicate (fromIntegral _entryDepth) "*"]
              ++ [" "]
              ++ [showKeyword kw <> " " | kw <- maybeToList _entryKeyword]
              ++ [ "(" <> c <> ") "
                   | c <- maybeToList _entryContext
                 ]
              ++ [_entryTitle]
              ++ [ " {" <> c <> "}"
                   | c <- maybeToList _entryLocator
                 ]
        suffix
          | not (null _entryTags) =
              T.concat $
                ":"
                  : [ case tag of
                        PlainTag t -> t <> ":"
                      | tag <- _entryTags
                    ]
          | otherwise = ""
    timestamps
      | null leadingStamps = []
      | otherwise = [T.intercalate " " (map showStamp leadingStamps)]
      where
        leadingStamps = filter isLeadingStamp _entryStamps
    properties
      | null _entryProperties = []
      | otherwise = showProperties propertyColumn _entryProperties
    logEntries = concatMap showLogEntry _entryLogEntries
    activeStamp = case _entryStamps ^? traverse . _ActiveStamp . _2 of
      Nothing -> []
      Just stamp -> [showTime stamp]
    entryLines = showBody "" _entryText

showBody :: Text -> Body -> [Text]
showBody leader (Body b) = concatMap (showBlock leader) b

showBlock :: Text -> Block -> [Text]
showBlock _leader (Whitespace _ txt) = [txt]
showBlock leader (Paragraph _ xs) = map (leader <>) xs
showBlock leader (Drawer _ xs) = map (leader <>) xs

bodyLength :: Body -> Int
bodyLength =
  sum . Prelude.map (fromIntegral . T.length) . showBody ""

showOrgFile :: Int -> Int -> OrgFile -> [Text]
showOrgFile propertyColumn tagsColumn OrgFile {..} =
  showHeader propertyColumn _fileHeader
    ++ concatMap (showEntry propertyColumn tagsColumn) _fileEntries

showHeader :: Int -> Header -> [Text]
showHeader propertyColumn Header {..} =
  propertiesDrawer
    ++ fileProperties
    ++ preamble
  where
    propertiesDrawer
      | null _headerPropertiesDrawer = []
      | otherwise = showProperties propertyColumn _headerPropertiesDrawer
    fileProperties
      | null _headerFileProperties = []
      | otherwise = showFileProperties _headerFileProperties
    preamble = showBody "" _headerPreamble

showProperties :: Int -> [Property] -> [Text]
showProperties propertyColumn ps =
  [":PROPERTIES:"]
    ++ map propLine ps
    ++ [":END:"]
  where
    propLine Property {..}
      | T.null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 1 = " "
          | otherwise = T.replicate (fromIntegral width) " "
          where
            width = propertyColumn - fromIntegral (T.length prefix)
        prefix = T.concat [":" <> _name <> ":"]
        suffix = _value

showFileProperties :: [Property] -> [Text]
showFileProperties ps =
  [ "#+" <> _name <> ": " <> _value
    | Property {..} <- ps
  ]

summarizeEntry :: Entry -> [Text]
summarizeEntry Entry {..} =
  [T.replicate (fromIntegral _entryDepth) "*" <> " " <> _entryTitle]
    ++ showProperties
      0
      ( _entryProperties
          ++ [Property _entryLoc False "FILE" (T.pack (_file _entryLoc))]
          ++ [Property _entryLoc False "LINE" (T.pack (show (_line _entryLoc)))]
          ++ [ Property _entryLoc False "KEYWORD" (T.pack (show x))
               | x <- maybeToList _entryKeyword
             ]
          ++ [ Property _entryLoc False "PRIORITY" x
               | x <- maybeToList _entryPriority
             ]
          ++ [ Property _entryLoc False "CONTEXT" x
               | x <- maybeToList _entryContext
             ]
          ++ [ Property _entryLoc False "LOCATOR" x
               | x <- maybeToList _entryLocator
             ]
          ++ [ Property
                 _entryLoc
                 False
                 "LOG_ENTRIES"
                 (T.pack (show (length _entryLogEntries)))
               | not (null _entryLogEntries)
             ]
          ++ [ Property
                 _entryLoc
                 False
                 "BODY_LEN"
                 (T.pack (show (bodyLength _entryText)))
               | not (emptyBody _entryText)
             ]
          ++ case _entryTags of
            [] -> []
            _ ->
              [ Property
                  _entryLoc
                  False
                  "TAGS"
                  ( T.concat $
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
                CreatedStamp _ tm ->
                  Property _entryLoc False "CREATED" (showTime tm)
                EditedStamp _ tm ->
                  Property _entryLoc False "EDITED" (showTime tm)
                DateStamp _ tm ->
                  Property _entryLoc False "DATE" (showTime tm)
                ActiveStamp _ tm ->
                  Property _entryLoc False "ACTIVE" (showTime tm)
            )
            _entryStamps
      )
    ++ concatMap summarizeEntry _entryItems
