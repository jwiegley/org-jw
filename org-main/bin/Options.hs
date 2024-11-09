{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Options where

import Data.Typeable (Typeable)
import FileTags.Options
import GHC.Generics
import JSON.Options
import Lint.Options
import Options.Applicative as OA
import Site.Options
import Stats.Options
import Trip.Options

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

tradeJournalSummary :: String
tradeJournalSummary =
  "org-jw "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data Command
  = Parse
  | Json JsonOptions
  | Print
  | Dump
  | Outline
  | Stats StatsOptions
  | Lint LintOptions
  | Tags TagsOptions
  | Test
  | Site SiteOptions
  | Trip TripOptions
  deriving (Show, Eq, Typeable, Generic)

data InputFiles
  = FileFromStdin -- '-f -'
  | ListFromStdin -- '-F -'
  | Paths [FilePath] -- '<path>...'
  | FilesFromFile FilePath -- '-F <path>'
  deriving (Show, Eq, Typeable, Generic, Ord)

data Options = Options
  { verbose :: !Bool,
    cacheDir :: !(Maybe FilePath),
    configFile :: !FilePath,
    keywordsGraph :: !(Maybe FilePath),
    command :: !Command,
    inputs :: !InputFiles
  }
  deriving (Show, Eq, Typeable, Generic)

tradeJournalOpts :: Parser Options
tradeJournalOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> optional
      ( strOption
          ( long "cache-dir"
              <> help "Directory to cache parsed Org-mode files"
          )
      )
    <*> strOption
      ( short 'c'
          <> long "config"
          <> help "Path to Yaml configuration file"
      )
    <*> optional
      ( strOption
          ( long "keywords"
              <> help "Keywords graph DOT file"
          )
      )
    <*> hsubparser
      ( parseCommand
          <> jsonCommand
          <> printCommand
          <> dumpCommand
          <> outlineCommand
          <> statsCommand
          <> lintCommand
          <> tagsCommand
          <> testCommand
          <> siteCommand
          <> tripCommand
      )
    <*> filesOptions
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
        (info (pure Parse) (progDesc "Parse Org-mode file"))

    jsonCommand :: Mod CommandFields Command
    jsonCommand =
      OA.command
        "json"
        (info (Json <$> jsonOptions) (progDesc "Output Org-mode file to JSON"))

    printCommand :: Mod CommandFields Command
    printCommand =
      OA.command
        "print"
        (info printOptions (progDesc "Print Org-mode file"))
      where
        printOptions :: Parser Command
        printOptions =
          pure Print

    dumpCommand :: Mod CommandFields Command
    dumpCommand =
      OA.command
        "dump"
        (info dumpOptions (progDesc "Dump Org-mode file"))
      where
        dumpOptions :: Parser Command
        dumpOptions =
          pure Dump

    outlineCommand :: Mod CommandFields Command
    outlineCommand =
      OA.command
        "outline"
        (info outlineOptions (progDesc "Outline Org-mode file"))
      where
        outlineOptions :: Parser Command
        outlineOptions =
          pure Outline

    statsCommand :: Mod CommandFields Command
    statsCommand =
      OA.command
        "stats"
        (info (Stats <$> statsOptions) (progDesc "Statistics on Org-mode files"))

    lintCommand :: Mod CommandFields Command
    lintCommand =
      OA.command
        "lint"
        (info (Lint <$> lintOptions) (progDesc "Lint Org-mode file"))

    tagsCommand :: Mod CommandFields Command
    tagsCommand =
      OA.command
        "tags"
        (info (Tags <$> tagsOptions) (progDesc "Org-mode filetags"))

    testCommand :: Mod CommandFields Command
    testCommand =
      OA.command
        "test"
        (info testOptions (progDesc "Test Org-mode file"))
      where
        testOptions :: Parser Command
        testOptions =
          pure Test

    siteCommand :: Mod CommandFields Command
    siteCommand =
      OA.command
        "site"
        (info (Site <$> siteOptions) (progDesc "Org-mode website builder"))

    tripCommand :: Mod CommandFields Command
    tripCommand =
      OA.command
        "trip"
        (info (Trip <$> tripOptions) (progDesc "Org-mode website builder"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
