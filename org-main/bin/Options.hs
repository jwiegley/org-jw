{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA
import Org.FileTags.Options
import Org.JSON.Options
import Org.Lint.Options
import Org.Read
import Org.Site.Options

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
  | Stats
  | Lint LintOptions
  | Tags TagsOptions
  | Test
  | Site SiteOptions
  deriving (Show, Typeable, Generic)

makeLenses ''Command

data Options = Options
  { _verbose :: !Bool,
    _command :: !Command,
    _inputs :: !InputFiles
  }
  deriving (Show, Typeable, Generic)

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
          <> jsonCommand
          <> printCommand
          <> dumpCommand
          <> outlineCommand
          <> statsCommand
          <> lintCommand
          <> tagsCommand
          <> testCommand
          <> siteCommand
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
        (info statsOptions (progDesc "Stats Org-mode file"))
      where
        statsOptions :: Parser Command
        statsOptions =
          pure Stats

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

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
