{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

data DBCommand
  = DBInit
  | DBStore
  | DBQuery DBQueryOpts
  deriving (Show, Eq, Typeable, Generic)

data DBQueryOpts = DBQueryOpts
  { _queryKeyword :: !(Maybe String)
  , _queryTag :: !(Maybe String)
  }
  deriving (Show, Eq, Typeable, Generic)

makeLenses ''DBQueryOpts

data DbOptions = DbOptions
  { _dbPath :: !FilePath
  , _dbCommand :: !DBCommand
  }
  deriving (Show, Eq, Typeable, Generic)

makeLenses ''DbOptions

dbOptions :: OA.Parser DbOptions
dbOptions =
  DbOptions
    <$> strOption
      ( short 'd'
          <> long "database"
          <> help "Path to SQLite database file"
          <> value "org.db"
          <> showDefault
      )
    <*> hsubparser
      ( initCommand
          <> storeCommand
          <> queryCommand
      )
 where
  initCommand =
    OA.command "init" (info (pure DBInit) (progDesc "Initialize database schema"))
  storeCommand =
    OA.command "store" (info (pure DBStore) (progDesc "Store org files into database"))
  queryCommand =
    OA.command "query" (info (DBQuery <$> queryOpts) (progDesc "Query the database"))

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
