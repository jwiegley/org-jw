{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Options where

import Control.Lens hiding (argument)
import Options.Applicative as OA
import Org.Lint
import Org.Read

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

data Options = Options
  { _verbose :: !Bool,
    _kind :: !LintMessageKind,
    _jsonDir :: !(Maybe FilePath),
    _inputs :: InputFiles
  }
  deriving (Show, Eq)

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
    <*> optional
      ( strOption
          ( long "to-json"
              <> help "Output Org-mode files as JSON to DIR"
          )
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
