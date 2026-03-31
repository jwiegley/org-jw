{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

------------------------------------------------------------------------
-- Data types (all defined before TH splices)
------------------------------------------------------------------------

data OutputFormat = TextFormat | JsonFormat | CsvFormat
  deriving (Show, Eq, Typeable, Generic)

data DBQueryOpts = DBQueryOpts
  { _queryKeyword :: !(Maybe String)
  , _queryTag :: !(Maybe String)
  , _queryOrgQl :: !(Maybe String)
  , _queryLimit :: !(Maybe Int)
  , _queryFormat :: !OutputFormat
  }
  deriving (Show, Eq, Typeable, Generic)

data DBSyncOpts = DBSyncOpts
  { _syncDirection :: !String
  , _syncFiles :: ![String]
  }
  deriving (Show, Eq, Typeable, Generic)

data DBDumpOpts = DBDumpOpts
  { _dumpTable :: !String
  , _dumpFormat :: !OutputFormat
  , _dumpLimit :: !(Maybe Int)
  }
  deriving (Show, Eq, Typeable, Generic)

data DBDotOpts = DBDotOpts
  { _dotOutput :: !(Maybe FilePath)
  , _dotRelTypes :: ![String]
  , _dotFilterFile :: ![String]
  , _dotFilterKw :: ![String]
  , _dotMaxDepthOpt :: !(Maybe Int)
  }
  deriving (Show, Eq, Typeable, Generic)

data DBCommand
  = DBInit
  | DBStore
  | DBQuery DBQueryOpts
  | DBSync DBSyncOpts
  | DBDump DBDumpOpts
  | DBDot DBDotOpts
  deriving (Show, Eq, Typeable, Generic)

data DbOptions = DbOptions
  { _dbConnStr :: !String
  , _dbCommand :: !DBCommand
  }
  deriving (Show, Eq, Typeable, Generic)

------------------------------------------------------------------------
-- TH lens generation (after all data type definitions)
------------------------------------------------------------------------

makeLenses ''DBQueryOpts
makeLenses ''DBSyncOpts
makeLenses ''DBDumpOpts
makeLenses ''DBDotOpts
makeLenses ''DbOptions

------------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------------

dbOptions :: OA.Parser DbOptions
dbOptions =
  DbOptions
    <$> strOption
      ( short 'u'
          <> long "db-url"
          <> help "PostgreSQL connection string"
          <> value "dbname=org_jw"
          <> showDefault
      )
    <*> hsubparser
      ( initCommand
          <> storeCommand
          <> queryCommand
          <> syncCommand
          <> dumpCommand
          <> dotCommand
      )
 where
  initCommand =
    OA.command "init" (info (pure DBInit) (progDesc "Initialize database schema"))
  storeCommand =
    OA.command "store" (info (pure DBStore) (progDesc "Store org files into database"))
  queryCommand =
    OA.command "query" (info (DBQuery <$> queryOpts) (progDesc "Query the database"))
  syncCommand =
    OA.command "sync" (info (DBSync <$> syncOpts) (progDesc "Sync org files with database"))
  dumpCommand =
    OA.command "dump" (info (DBDump <$> dumpOpts) (progDesc "Dump database table contents"))
  dotCommand =
    OA.command "dot" (info (DBDot <$> dotOpts) (progDesc "Generate Graphviz DOT from relationships"))

queryOpts :: OA.Parser DBQueryOpts
queryOpts =
  DBQueryOpts
    <$> optional
      ( strOption
          ( short 'k'
              <> long "keyword"
              <> help "Filter entries by keyword (e.g., TODO, DONE)"
          )
      )
    <*> optional
      ( strOption
          ( short 't'
              <> long "tag"
              <> help "Filter entries by tag"
          )
      )
    <*> optional
      ( strOption
          ( short 'q'
              <> long "ql"
              <> help "org-ql S-expression query"
          )
      )
    <*> optional
      ( option
          auto
          ( short 'n'
              <> long "limit"
              <> help "Maximum number of results"
          )
      )
    <*> formatOpt

syncOpts :: OA.Parser DBSyncOpts
syncOpts =
  DBSyncOpts
    <$> strOption
      ( long "direction"
          <> short 'd'
          <> help "Sync direction: to-db, from-db, or bidirectional"
          <> value "to-db"
          <> showDefault
      )
    <*> many
      ( strOption
          ( long "file"
              <> help "Specific file to sync (repeatable)"
          )
      )

dumpOpts :: OA.Parser DBDumpOpts
dumpOpts =
  DBDumpOpts
    <$> strOption
      ( long "table"
          <> short 't'
          <> help "Table to dump (files, entries, entry_tags, etc.)"
      )
    <*> formatOpt
    <*> optional
      ( option
          auto
          ( short 'n'
              <> long "limit"
              <> help "Maximum number of rows"
          )
      )

dotOpts :: OA.Parser DBDotOpts
dotOpts =
  DBDotOpts
    <$> optional
      ( strOption
          ( short 'o'
              <> long "output"
              <> help "Output file (stdout if omitted)"
          )
      )
    <*> many
      ( strOption
          ( long "rel-type"
              <> help "Relationship types to include (repeatable)"
          )
      )
    <*> many
      ( strOption
          ( long "filter-file"
              <> help "Only include entries from this file (repeatable)"
          )
      )
    <*> many
      ( strOption
          ( long "filter-keyword"
              <> help "Only include entries with this keyword (repeatable)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "max-depth"
              <> help "Maximum parent-child depth to include"
          )
      )

formatOpt :: OA.Parser OutputFormat
formatOpt =
  option
    readFormat
    ( long "format"
        <> short 'f'
        <> help "Output format: text, json, csv"
        <> value TextFormat
        <> showDefault
    )

readFormat :: OA.ReadM OutputFormat
readFormat = eitherReader $ \s -> case s of
  "text" -> Right TextFormat
  "json" -> Right JsonFormat
  "csv" -> Right CsvFormat
  _ -> Left ("Unknown format: " ++ s ++ ". Use text, json, or csv.")
