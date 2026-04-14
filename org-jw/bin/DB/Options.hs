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

data DBStoreOpts = DBStoreOpts
  { _storeNoEmbed :: !Bool
  , _storeEmbedOpts :: !DBEmbedOpts
  }
  deriving (Show, Eq, Typeable, Generic)

data DBEmbedOpts = DBEmbedOpts
  { _embedBaseUrlOpt :: !String
  , _embedModelOpt :: !String
  , _embedApiKeyOpt :: !String
  , _embedBatchSizeOpt :: !Int
  , _embedDimensionsOpt :: !(Maybe Int)
  , _embedChunkSizeOpt :: !Int
  , _embedForce :: !Bool
  }
  deriving (Show, Eq, Typeable, Generic)

data DBSearchOpts = DBSearchOpts
  { _searchQuery :: !String
  , _searchLimit :: !Int
  , _searchFormat :: !OutputFormat
  , _searchBaseUrlOpt :: !String
  , _searchModelOpt :: !String
  , _searchApiKeyOpt :: !String
  , _searchDimensionsOpt :: !(Maybe Int)
  }
  deriving (Show, Eq, Typeable, Generic)

data DBReviewOpts = DBReviewOpts
  { _reviewThreshold :: !Double
  , _reviewLimit :: !Int
  , _reviewFormat :: !OutputFormat
  }
  deriving (Show, Eq, Typeable, Generic)

data DBCommand
  = DBInit
  | DBStore DBStoreOpts
  | DBQuery DBQueryOpts
  | DBSync DBSyncOpts
  | DBDump DBDumpOpts
  | DBDot DBDotOpts
  | DBEmbed DBEmbedOpts
  | DBSearch DBSearchOpts
  | DBReview DBReviewOpts
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
makeLenses ''DBStoreOpts
makeLenses ''DBDumpOpts
makeLenses ''DBDotOpts
makeLenses ''DBEmbedOpts
makeLenses ''DBSearchOpts
makeLenses ''DBReviewOpts
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
          <> help "PostgreSQL connection string (default: use PGDATABASE/PGHOST/... env vars)"
          <> value ""
      )
    <*> hsubparser
      ( initCommand
          <> storeCommand
          <> queryCommand
          <> syncCommand
          <> dumpCommand
          <> dotCommand
          <> embedCommand
          <> searchCommand
          <> reviewCommand
      )
 where
  initCommand =
    OA.command "init" (info (pure DBInit) (progDesc "Initialize database schema"))
  storeCommand =
    OA.command "store" (info (DBStore <$> storeOpts) (progDesc "Store org files into database (embeds by default)"))
  queryCommand =
    OA.command "query" (info (DBQuery <$> queryOpts) (progDesc "Query the database"))
  syncCommand =
    OA.command "sync" (info (DBSync <$> syncOpts) (progDesc "Sync org files with database"))
  dumpCommand =
    OA.command "dump" (info (DBDump <$> dumpOpts) (progDesc "Dump database table contents"))
  dotCommand =
    OA.command "dot" (info (DBDot <$> dotOpts) (progDesc "Generate Graphviz DOT from relationships"))
  embedCommand =
    OA.command "embed" (info (DBEmbed <$> embedOpts) (progDesc "Generate vector embeddings for entries"))
  searchCommand =
    OA.command "search" (info (DBSearch <$> searchOpts) (progDesc "Semantic search over entries using embeddings"))
  reviewCommand =
    OA.command "review" (info (DBReview <$> reviewOpts) (progDesc "Find groups of entries with similar titles"))

storeOpts :: OA.Parser DBStoreOpts
storeOpts =
  DBStoreOpts
    <$> switch
      ( long "no-embed"
          <> help "Skip automatic embedding after store"
      )
    <*> embedOpts

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

embedOpts :: OA.Parser DBEmbedOpts
embedOpts =
  DBEmbedOpts
    <$> strOption
      ( long "base-url"
          <> help "Embedding API base URL (e.g., https://api.openai.com)"
          <> value "http://localhost:11434"
          <> showDefault
      )
    <*> strOption
      ( long "model"
          <> short 'm'
          <> help "Embedding model name"
          <> value "text-embedding-3-small"
          <> showDefault
      )
    <*> strOption
      ( long "api-key"
          <> help "API key (use any string for local servers)"
          <> value "unused"
          <> showDefault
      )
    <*> option
      auto
      ( long "batch-size"
          <> help "Number of texts per API call"
          <> value 50
          <> showDefault
      )
    <*> optional
      ( option
          auto
          ( long "dimensions"
              <> help "Override embedding dimensions (for text-embedding-3-* models)"
          )
      )
    <*> option
      auto
      ( long "chunk-size"
          <> help "Maximum characters per text chunk (entries are split into chunks for embedding)"
          <> value 2000
          <> showDefault
      )
    <*> switch
      ( long "force"
          <> help "Re-embed all entries, ignoring cached hashes"
      )

searchOpts :: OA.Parser DBSearchOpts
searchOpts =
  DBSearchOpts
    <$> strArgument
      ( metavar "QUERY"
          <> help "Search text"
      )
    <*> option
      auto
      ( short 'n'
          <> long "limit"
          <> help "Maximum number of results"
          <> value 10
          <> showDefault
      )
    <*> formatOpt
    <*> strOption
      ( long "base-url"
          <> help "Embedding API base URL"
          <> value "http://localhost:11434"
          <> showDefault
      )
    <*> strOption
      ( long "model"
          <> short 'm'
          <> help "Embedding model name (must match the model used for db embed)"
          <> value "text-embedding-3-small"
          <> showDefault
      )
    <*> strOption
      ( long "api-key"
          <> help "API key"
          <> value "unused"
          <> showDefault
      )
    <*> optional
      ( option
          auto
          ( long "dimensions"
              <> help "Override embedding dimensions"
          )
      )

reviewOpts :: OA.Parser DBReviewOpts
reviewOpts =
  DBReviewOpts
    <$> option
      auto
      ( long "threshold"
          <> help "Maximum cosine distance to consider titles similar (0.0-2.0)"
          <> value 0.15
          <> showDefault
      )
    <*> option
      auto
      ( short 'n'
          <> long "limit"
          <> help "Maximum number of pairs to consider for grouping"
          <> value 1000
          <> showDefault
      )
    <*> formatOpt

readFormat :: OA.ReadM OutputFormat
readFormat = eitherReader $ \s -> case s of
  "text" -> Right TextFormat
  "json" -> Right JsonFormat
  "csv" -> Right CsvFormat
  _ -> Left ("Unknown format: " ++ s ++ ". Use text, json, or csv.")
