{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Org.FileTags.Options where

import Control.Lens hiding (List, argument)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Typeable (Typeable)
import FlatParse.Combinators
import GHC.Generics
import Options.Applicative as OA
import Org.FileTags.Filter

data TagsCommand
  = List
  | TagTrees
      { tagTreesDryRun :: Bool,
        tagTreesDirectory :: FilePath,
        tagTreesOverwrite :: Bool,
        tagTreesDepth :: Int,
        tagTreesTagForUntagged :: Maybe String
      }
  | Filter
      { filterDryRun :: Bool,
        filterDirectory :: FilePath,
        filterOverwrite :: Bool,
        filterExpr :: TagExpr
      }
  deriving (Show, Typeable, Generic)

makePrisms ''TagsCommand

data TagsOptions = TagsOptions
  { _command :: !TagsCommand
  }
  deriving (Show, Typeable, Generic)

makeLenses ''TagsOptions

tagsOptions :: Parser TagsOptions
tagsOptions =
  TagsOptions
    <$> hsubparser
      ( tagsCommand
          <> tagTreesCommand
          <> filterCommand
      )
  where
    tagsCommand :: Mod CommandFields TagsCommand
    tagsCommand =
      OA.command
        "list"
        (info listOptions (progDesc "Org-mode file tags"))
      where
        listOptions :: Parser TagsCommand
        listOptions = pure List

    tagTreesCommand :: Mod CommandFields TagsCommand
    tagTreesCommand =
      OA.command
        "trees"
        (info tagTreesOptions (progDesc "Create tag trees"))
      where
        tagTreesOptions :: Parser TagsCommand
        tagTreesOptions =
          TagTrees
            <$> switch
              ( short 'n'
                  <> long "dry-run"
                  <> help "If enabled, make no changes to disk"
              )
            <*> option
              auto
              ( long "directory"
                  <> value ".tagtrees"
                  <> help "Directory to create tag trees in"
              )
            <*> switch
              ( short 'f'
                  <> long "force"
                  <> help "If enabled, remove existing tagtrees directory"
              )
            <*> option
              auto
              ( short 'd'
                  <> long "depth"
                  <> value 2
                  <> help "Depth of tag hierarchy to create"
              )
            <*> optional
              ( strOption
                  ( long "tag-for-untagged"
                      <> help "Depth of tag hierarchy to create"
                  )
              )

    filterCommand :: Mod CommandFields TagsCommand
    filterCommand =
      OA.command
        "filter"
        (info filterOptions (progDesc "Filter by tag expression"))
      where
        filterOptions :: Parser TagsCommand
        filterOptions =
          Filter
            <$> switch
              ( short 'n'
                  <> long "dry-run"
                  <> help "If enabled, make no changes to disk"
              )
            <*> option
              auto
              ( long "directory"
                  <> value ".filter"
                  <> help "Directory to create tag trees in"
              )
            <*> switch
              ( short 'f'
                  <> long "force"
                  <> help "If enabled, remove existing filter directory"
              )
            <*> option
              ( maybeReader
                  ( parseMaybe () parseTagExpr
                      . T.encodeUtf8
                      . T.pack
                  )
              )
              ( short 't'
                  <> long "tags"
                  <> help "Tags to filter by"
              )
