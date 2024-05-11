{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

version :: String
version = "0.0.1"

copyright :: String
copyright = "2020"

tradeJournalSummary :: String
tradeJournalSummary =
  "org-data "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data Command
  = Parse
  | Print
  | Dump
  | Outline
  | Stats
  | Lint
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Command

data InputFiles
  = FileFromStdin
  | ListFromStdin
  | SingleFile FilePath
  | FilesFromFile FilePath
  deriving (Data, Show, Eq, Typeable, Generic)

data Options = Options
  { _verbose :: !Bool,
    _paths :: !InputFiles,
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
    <*> ( ( ( \x ->
                if x == "-"
                  then FileFromStdin
                  else SingleFile x
            )
              <$> strOption
                ( short 'f'
                    <> long "file"
                    <> help "Single file to process"
                )
          )
            <|> ( ( \x ->
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
        )
    <*> hsubparser
      ( parseCommand
          <> printCommand
          <> dumpCommand
          <> outlineCommand
          <> statsCommand
          <> lintCommand
      )
  where
    parseCommand :: Mod CommandFields Command
    parseCommand =
      OA.command
        "parse"
        (info parseOptions (progDesc "Parse Org-mode file"))
      where
        parseOptions :: Parser Command
        parseOptions =
          pure Parse

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
        (info lintOptions (progDesc "Lint Org-mode file"))
      where
        lintOptions :: Parser Command
        lintOptions =
          pure Lint

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
