{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Print where

import Control.Lens
import Data.Maybe (maybeToList)
import Data.Time
import Org.Types hiding (propertyColumn, tagsColumn)

showStamp :: Stamp -> String
showStamp (ClosedStamp _ tm) = "CLOSED: " <> showTime tm
showStamp (ScheduledStamp _ tm) = "SCHEDULED: " <> showTime tm
showStamp (DeadlineStamp _ tm) = "DEADLINE: " <> showTime tm
showStamp x = error $ "showStamp not support for " ++ show x

showTime :: Time -> String
showTime tm =
  concat $
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

showTimeSingle :: Time -> String
showTimeSingle Time {..} =
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
        Nothing -> []
        Just finish ->
          [ formatTime
              defaultTimeLocale
              "-%H:%M"
              ( UTCTime
                  (ModifiedJulianDay _timeDay)
                  (secondsToDiffTime (finish * 60))
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
            show _suffixNum,
            case _suffixSpan of
              DaySpan -> "d"
              WeekSpan -> "w"
              MonthSpan -> "m"
          ]
            ++ case _suffixLargerSpan of
              Nothing -> []
              Just (num, s) ->
                [ show num,
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

showDuration :: Duration -> String
showDuration Duration {..} =
  pad ' ' (show _hours) <> pad '0' (show _mins)
  where
    pad c [x] = [c, x]
    pad _ xs = xs

showLogEntry :: LogEntry -> [String]
showLogEntry (LogClosing _ tm text) =
  ( "- CLOSING NOTE"
      <> showTime tm
      <> if null text then "" else " \\\\"
  )
    : maybe [] (showBody "  ") text
showLogEntry (LogState _ fr t tm text) =
  concat
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

showKeyword :: Keyword -> String
showKeyword (OpenKeyword _ n) = n
showKeyword (ClosedKeyword _ n) = n

showEntry :: Int -> Int -> Entry -> [String]
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
      | null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 2 = "  "
          | otherwise = replicate (fromIntegral width) ' '
          where
            width =
              tagsColumn
                - fromIntegral (length prefix)
                - fromIntegral (length suffix)
        prefix =
          concat $
            [replicate (fromIntegral _entryDepth) '*']
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
      | null _entryProperties = []
      | otherwise = showProperties propertyColumn _entryProperties
    logEntries = concatMap showLogEntry _entryLogEntries
    activeStamp = case _entryStamps ^? traverse . _ActiveStamp . _2 of
      Nothing -> []
      Just stamp -> [showTime stamp]
    entryLines = showBody "" _entryString

showBody :: String -> Body -> [String]
showBody leader (Body b) = concatMap (showBlock leader) b

showBlock :: String -> Block -> [String]
showBlock _leader (Whitespace _ txt) = [txt]
showBlock leader (Paragraph _ xs) = map (leader <>) xs
showBlock leader (Drawer _ xs) = map (leader <>) xs

bodyLength :: Body -> Int
bodyLength =
  sum . Prelude.map (fromIntegral . length) . showBody ""

showOrgFile :: Int -> Int -> OrgFile -> [String]
showOrgFile propertyColumn tagsColumn OrgFile {..} =
  showHeader propertyColumn _orgFileHeader
    ++ concatMap (showEntry propertyColumn tagsColumn) _orgFileEntries

showHeader :: Int -> Header -> [String]
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

showProperties :: Int -> [Property] -> [String]
showProperties propertyColumn ps =
  [":PROPERTIES:"]
    ++ map propLine ps
    ++ [":END:"]
  where
    propLine Property {..}
      | null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 1 = " "
          | otherwise = replicate (fromIntegral width) ' '
          where
            width = propertyColumn - fromIntegral (length prefix)
        prefix = ":" <> _name <> ":"
        suffix = _value

showFileProperties :: [Property] -> [String]
showFileProperties ps =
  [ "#+" <> _name <> ": " <> _value
    | Property {..} <- ps
  ]

summarizeEntry :: Entry -> [String]
summarizeEntry Entry {..} =
  [replicate (fromIntegral _entryDepth) '*' <> " " <> _entryTitle]
    ++ showProperties
      0
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
                 (show (bodyLength _entryString))
               | not (emptyBody _entryString)
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
