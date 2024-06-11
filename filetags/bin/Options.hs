{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Typeable (Typeable)
import FlatParse.Combinators
import GHC.Generics
import Options.Applicative as OA
import Org.Filter

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

filetagsSummary :: String
filetagsSummary =
  "filetags "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data InputFiles
  = FileFromStdin -- '-f -'
  | ListFromStdin -- '-F -'
  | Paths [FilePath] -- '<path>...'
  | FilesFromFile FilePath -- '-F <path>'
  deriving (Data, Show, Eq, Typeable, Generic)

makePrisms ''InputFiles

data Command
  = TagsList InputFiles
  | TagTrees
      { tagTreesDryRun :: Bool,
        tagTreesDirectory :: FilePath,
        tagTreesOverwrite :: Bool,
        tagTreesDepth :: Int,
        tagTreesTagForUntagged :: Maybe String,
        tagTreesInputs :: InputFiles
      }
  | Filter
      { filterDryRun :: Bool,
        filterDirectory :: FilePath,
        filterOverwrite :: Bool,
        filterExpr :: TagExpr,
        filterInputs :: InputFiles
      }
  deriving (Data, Show, Eq, Typeable, Generic)

makePrisms ''Command

commandInput :: Lens' Command InputFiles
commandInput f (TagsList input) = TagsList <$> f input
commandInput f (TagTrees dryRun dir overwrite depth tagForUntagged input) =
  TagTrees dryRun dir overwrite depth tagForUntagged <$> f input
commandInput f (Filter dryRun dir overwrite expr input) =
  Filter dryRun dir overwrite expr <$> f input

data Options = Options
  { _verbose :: !Bool,
    _command :: !Command
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Options

filetagsOpts :: Parser Options
filetagsOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> hsubparser
      ( tagsCommand
          <> tagTreesCommand
          <> filterCommand
      )
  where
    filesOptions =
      ( ( \x ->
            if x == "-"
              then ListFromStdin
              else FilesFromFile x
        )
          <$> strOption
            ( short 'F'
                <> long "files"
                <> help "List of files to process"
            )
      )
        <|> ( ( \xs ->
                  if xs == ["-"]
                    then FileFromStdin
                    else Paths xs
              )
                <$> some (argument str (metavar "FILES"))
            )

    tagsCommand :: Mod CommandFields Command
    tagsCommand =
      OA.command
        "tags-list"
        (info tagsOptions (progDesc "Org-mode file tags"))
      where
        tagsOptions :: Parser Command
        tagsOptions = TagsList <$> filesOptions

    tagTreesCommand :: Mod CommandFields Command
    tagTreesCommand =
      OA.command
        "tagtrees"
        (info tagTreesOptions (progDesc "Create tag trees"))
      where
        tagTreesOptions :: Parser Command
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
            <*> filesOptions

    filterCommand :: Mod CommandFields Command
    filterCommand =
      OA.command
        "filter"
        (info filterOptions (progDesc "Filter by tag expression"))
      where
        filterOptions :: Parser Command
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
                  ( parseMaybe parseTagExpr
                      . T.encodeUtf8
                      . T.pack
                  )
              )
              ( short 't'
                  <> long "tags"
                  <> help "Tags to filter by"
              )
            <*> filesOptions

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> filetagsOpts)
    (fullDesc <> progDesc "" <> header filetagsSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
