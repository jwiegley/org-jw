{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Org.Printer where

import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Time
import Org.Types

showOrgStamp :: OrgStamp -> Text
showOrgStamp OrgStamp {..} =
  T.concat
    [ case orgStampKind of
        ClosedStamp -> "CLOSED: "
        ScheduledStamp -> "SCHEDULED: "
        DeadlineStamp -> "DEADLINE: ",
      showOrgTime orgStampTime
    ]

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
  where
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

showOrgLogEntry :: OrgLogEntry -> [Text]
showOrgLogEntry (OrgLogStateChange from to time text) =
  [ "- State \""
      <> showOrgKeyword from
      <> "\" from \""
      <> showOrgKeyword to
      <> "\" "
      <> showOrgTime time
      <> if null text then "" else " \\\\"
  ]
    ++ map ("  " <>) text
showOrgLogEntry (OrgLogNote time text) =
  [ "- Note taken on "
      <> showOrgTime time
      <> if null text then "" else " \\\\"
  ]
    ++ map ("  " <>) text

showOrgKeyword :: OrgKeyword -> Text
showOrgKeyword (OpenKeyword n) = n
showOrgKeyword (ClosedKeyword n) = n

showOrgEntry :: Int -> Int -> OrgEntry -> [Text]
showOrgEntry propertyColumn tagsColumn OrgEntry {..} =
  [title]
    ++ timestamps
    ++ properties
    ++ logEntries
    ++ body
    ++ concatMap (showOrgEntry propertyColumn tagsColumn) entryItems
  where
    title
      | T.null suffix = prefix
      | otherwise = prefix <> spacer <> suffix
      where
        spacer
          | width < 2 = "  "
          | otherwise = T.replicate width " "
          where
            width = tagsColumn - T.length prefix - T.length suffix
        prefix =
          T.concat $
            [T.replicate entryDepth "*"]
              ++ [" "]
              ++ [showOrgKeyword kw <> " " | kw <- maybeToList entryKeyword]
              ++ [ "(" <> c <> ") "
                   | c <- maybeToList entryContext
                 ]
              ++ [entryTitle]
              ++ [ " {" <> c <> "}"
                   | c <- maybeToList entryLocator
                 ]
        suffix
          | not (null entryTags) =
              T.concat $
                [":"]
                  ++ [ case tag of
                         OrgSpecialTag t -> t <> ":"
                         OrgPlainTag t -> t <> ":"
                       | tag <- entryTags
                     ]
          | otherwise = ""
    timestamps
      | null entryStamps = []
      | otherwise = [T.intercalate " " (map showOrgStamp entryStamps)]
    properties
      | null entryProperties = []
      | otherwise = showProperties propertyColumn entryProperties
    logEntries = concatMap showOrgLogEntry entryLogEntries
    body = entryText

showOrgFile :: Int -> Int -> OrgFile -> [Text]
showOrgFile propertyColumn tagsColumn OrgFile {..} =
  showOrgHeader propertyColumn fileHeader
    ++ concatMap (showOrgEntry propertyColumn tagsColumn) fileEntries

showOrgHeader :: Int -> OrgHeader -> [Text]
showOrgHeader propertyColumn OrgHeader {..} =
  propertiesDrawer
    ++ fileProperties
    ++ preamble
  where
    propertiesDrawer
      | null headerPropertiesDrawer = []
      | otherwise = showProperties propertyColumn headerPropertiesDrawer
    fileProperties
      | null headerFileProperties = []
      | otherwise = showFileProperties headerFileProperties
    preamble = headerPreamble

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
          | otherwise = T.replicate width " "
          where
            width = propertyColumn - T.length prefix
        prefix = T.concat [":" <> propertyName <> ":"]
        suffix = propertyValue

showFileProperties :: [Property] -> [Text]
showFileProperties ps =
  [ "#+" <> propertyName <> ": " <> propertyValue
    | Property {..} <- ps
  ]

summarizeEntry :: OrgEntry -> [Text]
summarizeEntry OrgEntry {..} =
  [T.replicate entryDepth "*" <> " " <> entryTitle]
    ++ showProperties
      0
      ( entryProperties
          ++ [Property "ORIGIN" (T.pack (show entryPos))]
          ++ [ Property "KEYWORD" (T.pack (show x))
               | x <- maybeToList entryKeyword
             ]
          ++ [Property "PRIORITY" x | x <- maybeToList entryPriority]
          ++ [Property "CONTEXT" x | x <- maybeToList entryContext]
          ++ [Property "LOCATOR" x | x <- maybeToList entryLocator]
          ++ [ Property "LOG_LEN" (T.pack (show (length entryLogEntries)))
               | not (null entryLogEntries)
             ]
          ++ [ Property "BODY_LEN" (T.pack (show (length entryText)))
               | not (null entryText)
             ]
          ++ case entryTags of
            [] -> []
            _ ->
              [ Property
                  "TAGS"
                  ( T.concat $
                      [":"]
                        ++ [ case tag of
                               OrgSpecialTag t -> t <> ":"
                               OrgPlainTag t -> t <> ":"
                             | tag <- entryTags
                           ]
                  )
              ]
          ++ map
            ( \OrgStamp {..} ->
                Property
                  ( case orgStampKind of
                      ClosedStamp -> "CLOSED"
                      ScheduledStamp -> "SCHEDULED"
                      DeadlineStamp -> "DEADLINE"
                  )
                  (showOrgTime orgStampTime)
            )
            entryStamps
      )
    ++ concatMap summarizeEntry entryItems
