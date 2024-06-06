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
import GHC.Generics
import Options.Applicative as OA
import Org.Filter
import Org.Lint
import Org.Parser

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

tradeJournalSummary :: String
tradeJournalSummary =
  "org-data "
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
  = Parse InputFiles
  | TagsList InputFiles
  | CategoriesList InputFiles
  | Print InputFiles
  | Dump InputFiles
  | Outline InputFiles
  | Stats InputFiles
  | Lint LintMessageKind InputFiles
  | Test InputFiles
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
commandInput f (Parse input) = Parse <$> f input
commandInput f (TagsList input) = TagsList <$> f input
commandInput f (CategoriesList input) = CategoriesList <$> f input
commandInput f (Print input) = Print <$> f input
commandInput f (Dump input) = Dump <$> f input
commandInput f (Outline input) = Outline <$> f input
commandInput f (Stats input) = Stats <$> f input
commandInput f (Lint kind input) = Lint kind <$> f input
commandInput f (Test input) = Test <$> f input
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

tradeJournalOpts :: Parser Options
tradeJournalOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> hsubparser
      ( parseCommand
          <> tagsCommand
          <> categoriesCommand
          <> printCommand
          <> dumpCommand
          <> outlineCommand
          <> statsCommand
          <> lintCommand
          <> testCommand
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

    parseCommand :: Mod CommandFields Command
    parseCommand =
      OA.command
        "parse"
        (info parseOptions (progDesc "Parse Org-mode file"))
      where
        parseOptions :: Parser Command
        parseOptions = Parse <$> filesOptions

    tagsCommand :: Mod CommandFields Command
    tagsCommand =
      OA.command
        "tags-list"
        (info tagsOptions (progDesc "Org-mode file tags"))
      where
        tagsOptions :: Parser Command
        tagsOptions = TagsList <$> filesOptions

    categoriesCommand :: Mod CommandFields Command
    categoriesCommand =
      OA.command
        "categories-list"
        (info categoriesOptions (progDesc "Org-mode file categories"))
      where
        categoriesOptions :: Parser Command
        categoriesOptions = CategoriesList <$> filesOptions

    printCommand :: Mod CommandFields Command
    printCommand =
      OA.command
        "print"
        (info printOptions (progDesc "Print Org-mode file"))
      where
        printOptions :: Parser Command
        printOptions = Print <$> filesOptions

    dumpCommand :: Mod CommandFields Command
    dumpCommand =
      OA.command
        "dump"
        (info dumpOptions (progDesc "Dump Org-mode file"))
      where
        dumpOptions :: Parser Command
        dumpOptions = Dump <$> filesOptions

    outlineCommand :: Mod CommandFields Command
    outlineCommand =
      OA.command
        "outline"
        (info outlineOptions (progDesc "Outline Org-mode file"))
      where
        outlineOptions :: Parser Command
        outlineOptions = Outline <$> filesOptions

    statsCommand :: Mod CommandFields Command
    statsCommand =
      OA.command
        "stats"
        (info statsOptions (progDesc "Stats Org-mode file"))
      where
        statsOptions :: Parser Command
        statsOptions = Stats <$> filesOptions

    lintCommand :: Mod CommandFields Command
    lintCommand =
      OA.command
        "lint"
        (info lintOptions (progDesc "Lint Org-mode file"))
      where
        lintOptions :: Parser Command
        lintOptions =
          Lint
            <$> option
              ( maybeReader
                  ( parseMaybe parseLintMessageKind
                      . T.encodeUtf8
                      . T.pack
                  )
              )
              ( short 'l'
                  <> long "level"
                  <> value LintInfo
                  <> help "Log level to report"
              )
            <*> filesOptions

    testCommand :: Mod CommandFields Command
    testCommand =
      OA.command
        "test"
        (info testOptions (progDesc "Test Org-mode file"))
      where
        testOptions :: Parser Command
        testOptions = Test <$> filesOptions

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
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
