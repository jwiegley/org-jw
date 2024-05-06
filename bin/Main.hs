{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Control.Monad.Reader
import Data.Set qualified as S
import Data.Text.IO qualified as T
import GHC.Generics
import Options qualified
import Org.Parser
import Org.Printer
import Org.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Show.Pretty

data Config = Config
  {
  }
  deriving (Generic, Show)

newConfig :: Config
newConfig =
  Config
    {
    }

main :: IO ()
main = do
  opts <- Options.getOptions
  case opts ^. Options.command of
    Options.Parse path -> do
      content <- T.readFile path
      case runReader (runParserT parseOrg path content) OrgConfig {..} of
        Left bundle -> putStr $ errorBundlePretty bundle
        Right org -> do
          putStrLn $
            "There are "
              <> show (length (fileEntries org))
              <> " org-mode entries"
          putStrLn "Tags found:"
          mapM_
            ( \case
                OrgPlainTag tag -> T.putStrLn $ "  " <> tag
                OrgSpecialTag tag -> T.putStrLn $ "  [" <> tag <> "]"
            )
            ( foldr
                S.insert
                mempty
                (concatMap entryTags (fileEntries org))
            )
          putStrLn "Contexts found:"
          mapM_
            ( \case
                Nothing -> pure ()
                Just x -> T.putStrLn $ "  " <> x
            )
            ( foldr
                S.insert
                mempty
                (map entryContext (fileEntries org))
            )
          putStrLn "Locations found:"
          mapM_
            ( \case
                Nothing -> pure ()
                Just x -> T.putStrLn $ "  " <> x
            )
            ( foldr
                S.insert
                mempty
                (map entryLocator (fileEntries org))
            )
          mapM_
            ( \title -> case parseMaybe
                (count 4 upperChar :: BasicParser String)
                title of
                Nothing -> pure ()
                Just _ -> T.putStrLn $ "Fishy title: " <> title
            )
            ( foldr
                S.insert
                mempty
                (map entryTitle (fileEntries org))
            )
    Options.Print path ->
      processFile path $
        mapM_ T.putStrLn . showOrgFile propertyColumn tagsColumn
    Options.Dump path ->
      processFile path pPrint
    Options.Outline path ->
      processFile path $
        mapM_ T.putStrLn
          . concatMap (summarizeEntry propertyColumn)
          . fileEntries
  where
    processFile path f = do
      content <- T.readFile path
      case runReader (runParserT parseOrg path content) OrgConfig {..} of
        Left bundle -> putStr $ errorBundlePretty bundle
        Right org -> f org
    openKeywords =
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
    closedKeywords =
      [ "DONE",
        "CANCELED",
        "NOTE",
        "LINK"
      ]
    priorities =
      ["A", "B", "C"]
    specialTags =
      ["ARCHIVE", "FILE", "URL"]
    propertyColumn = 11
    tagsColumn = 97
