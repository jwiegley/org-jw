{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Control.Monad.Reader
import Data.Text.IO qualified as T
import GHC.Generics
import Options qualified
import Org.Parser
import Text.Megaparsec

-- import Text.Show.Pretty

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
      putStrLn $ "Reading Org-mode file " ++ path
      content <- T.readFile path
      let openKeywords =
            [ "TODO",
              "CATEGORY",
              "PROJECT",
              "STARTED",
              "WAITING",
              "DEFERRED"
            ]
          closedKeywords =
            [ "DONE",
              "CANCELED",
              "NOTE",
              "LINK"
            ]
          priorities =
            ["A", "B", "C"]
      case runReader (runParserT parseOrg path content) OrgConfig {..} of
        Left bundle -> putStr $ errorBundlePretty bundle
        Right org -> do
          putStrLn $
            "There are "
              <> show (length (fileEntries org))
              <> " org-mode entries"
