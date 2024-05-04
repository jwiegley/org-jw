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
  = Parse !FilePath
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Command

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
    <*> hsubparser parseCommand
  where
    parseCommand :: Mod CommandFields Command
    parseCommand =
      OA.command
        "parse"
        (info parseOptions (progDesc "Parse Org-mode file"))
      where
        parseOptions :: Parser Command
        parseOptions =
          Parse
            <$> strArgument (metavar "FILE" <> help "Org-mode file to read")

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
