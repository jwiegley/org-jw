{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA
import Org.Lint

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

orgLintSummary :: String
orgLintSummary =
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

data Options = Options
  { _verbose :: !Bool,
    _kind :: !LintMessageKind,
    _inputs :: InputFiles
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Options

orgLintOpts :: Parser Options
orgLintOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> option
      (maybeReader parseLintMessageKind)
      ( short 'l'
          <> long "level"
          <> value LintInfo
          <> help "Log level to report"
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

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> orgLintOpts)
    (fullDesc <> progDesc "" <> header orgLintSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
