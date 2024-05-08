{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text.IO qualified as T
import Options qualified
import Org.Data
import Org.Printer
import Org.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Show.Pretty

main :: IO ()
main = do
  opts <- Options.getOptions
  case opts ^. Options.command of
    Options.Parse path ->
      processFile path $ \org -> do
        putStrLn $
          "There are "
            <> show (length (_fileEntries org))
            <> " org-mode entries"
        putStrLn "Tags found:"
        mapM_
          ( \case
              PlainTag tag -> T.putStrLn $ "  " <> tag
              SpecialTag tag -> T.putStrLn $ "  [" <> tag <> "]"
          )
          ( foldr
              S.insert
              mempty
              (concatMap _entryTags (_fileEntries org))
          )
        putStrLn "Contexts found:"
        mapM_
          ( \case
              Nothing -> pure ()
              Just x -> T.putStrLn $ "  " <> x
          )
          (foldr (S.insert . _entryContext) mempty (_fileEntries org))
        putStrLn "Locations found:"
        mapM_
          ( \case
              Nothing -> pure ()
              Just x -> T.putStrLn $ "  " <> x
          )
          (foldr (S.insert . _entryLocator) mempty (_fileEntries org))
        mapM_
          ( \title -> case parseMaybe
              (count 4 upperChar :: BasicParser String)
              title of
              Nothing -> pure ()
              Just _ -> T.putStrLn $ "Fishy title: " <> title
          )
          (foldr (S.insert . _entryTitle) mempty (_fileEntries org))
    Options.Print path ->
      processFile path $
        mapM_ T.putStrLn . showOrgFile _propertyColumn _tagsColumn
    Options.Dump path ->
      processFile path $ \org -> do
        pPrint org
        pPrint $ entriesMap [] OrgData {_orgFiles = M.singleton path org}
    Options.Outline path ->
      processFile path $
        mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries
  where
    processFile path f = do
      content <- T.readFile path
      case content ^? _orgFile path Config {..} of
        Nothing -> error $ "Cannot parse: " ++ path
        Just org -> f org
    _openKeywords =
      [ "TODO",
        "CATEGORY",
        "PROJECT",
        "STARTED",
        "WAITING",
        "DEFERRED",
        "SOMEDAY",
        "DELEGATED",
        "APPT"
      ]
    _closedKeywords =
      [ "DONE",
        "CANCELED",
        "NOTE",
        "LINK"
      ]
    _priorities =
      ["A", "B", "C"]
    _specialTags =
      ["ARCHIVE", "FILE", "URL"]
    _propertyColumn = 11
    _tagsColumn = 97
