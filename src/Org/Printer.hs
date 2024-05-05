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

summarizeEntry :: OrgEntry -> [Text]
summarizeEntry OrgEntry {..} =
  [ T.replicate entryDepth "*" <> " " <> entryTitle,
    ":PROPERTIES:"
  ]
    ++ [":KEYWORD: " <> T.pack (show x) | x <- maybeToList entryKeyword]
    ++ [":PRIORITY: " <> x | x <- maybeToList entryPriority]
    ++ [":CONTEXT: " <> x | x <- maybeToList entryContext]
    ++ [":LOCATOR: " <> x | x <- maybeToList entryLocator]
    ++ [ ":TEXT_LENGTH: " <> T.pack (show (length xs))
         | xs <- maybeToList entryText
       ]
    ++ [ ":" <> propertyName <> ": " <> propertyValue
         | Property {..} <- entryProperties
       ]
    ++ case entryTags of
      [] -> []
      _ ->
        [ ":TAGS: :"
            <> T.concat
              [ case tag of
                  OrgSpecialTag t -> t <> ":"
                  OrgPlainTag t -> t <> ":"
                | tag <- entryTags
              ]
        ]
    ++ map
      ( \OrgStamp {..} ->
          ( case orgStampKind of
              ClosedStamp -> ":CLOSED: "
              ScheduledStamp -> ":SCHEDULED: "
              DeadlineStamp -> ":DEADLINE: "
          )
            <> showOrgTime orgStampTime
      )
      entryStamps
    ++ [":END:"]
    ++ concatMap summarizeEntry entryItems
