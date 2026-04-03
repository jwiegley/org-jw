{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Org.DB.Sync (
  -- * Sync operations
  syncToDb,
  syncFromDb,
  syncBidirectional,

  -- * Entry ID management
  ensureEntryId,

  -- * Results
  SyncResult (..),
  SyncConflict (..),
  SyncDirection (..),
) where

import Control.Lens hiding ((<.>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Org.DB.Deserialize
import Org.DB.Store
import Org.DB.Types
import Org.Data
import Org.Types
import System.Directory (getModificationTime)

-- | Direction for sync operations.
data SyncDirection = ToDb | FromDb | Bidirectional
  deriving (Show, Eq)

-- | A conflict detected during bidirectional sync.
data SyncConflict = SyncConflict
  { conflictFile :: Text
  , conflictReason :: Text
  , conflictDbTime :: Maybe UTCTime
  , conflictFileTime :: UTCTime
  }
  deriving (Show, Eq)

-- | Result of a sync operation.
data SyncResult = SyncResult
  { syncFilesProcessed :: Int
  , syncFilesSkipped :: Int
  , syncFilesUpdated :: Int
  , syncConflicts :: [SyncConflict]
  , syncErrors :: [Text]
  }
  deriving (Show, Eq)

emptySyncResult :: SyncResult
emptySyncResult = SyncResult 0 0 0 [] []

{- | Sync org files to database. Org files are source of truth.
This is a thin wrapper around storeCollection that provides
detailed reporting.
-}
syncToDb :: DBHandle -> Collection -> IO SyncResult
syncToDb db coll = do
  let orgFiles = coll ^.. items . traverse . _OrgItem
  results <- mapM (syncOneFileToDb db) orgFiles
  pure (foldr mergeSyncResult emptySyncResult results)

syncOneFileToDb :: DBHandle -> OrgFile -> IO SyncResult
syncOneFileToDb db org = do
  let path = org ^. orgFilePath
      pathText = T.pack path
  mtime <- getModificationTime path
  existing <- queryFileByPath db pathText
  case existing of
    Just row
      | Just mt <- frModTime row
      , mt >= mtime ->
          pure emptySyncResult{syncFilesProcessed = 1, syncFilesSkipped = 1}
    _ -> do
      _ <- storeOrgFile db org
      pure emptySyncResult{syncFilesProcessed = 1, syncFilesUpdated = 1}

-- | Load the entire database as a Collection. DB is source of truth.
syncFromDb :: DBHandle -> IO Collection
syncFromDb = loadCollection

{- | Bidirectional sync using modification times.
Files where DB mod_time > file mod_time produce conflicts.
Files where file mod_time > DB mod_time are synced to DB.
-}
syncBidirectional :: DBHandle -> Collection -> IO SyncResult
syncBidirectional db coll = do
  let orgFiles = coll ^.. items . traverse . _OrgItem
  results <- mapM (syncBidirectionalFile db) orgFiles
  pure (foldr mergeSyncResult emptySyncResult results)

syncBidirectionalFile :: DBHandle -> OrgFile -> IO SyncResult
syncBidirectionalFile db org = do
  let path = org ^. orgFilePath
      pathText = T.pack path
  mtime <- getModificationTime path
  existing <- queryFileByPath db pathText
  case existing of
    Nothing -> do
      _ <- storeOrgFile db org
      pure emptySyncResult{syncFilesProcessed = 1, syncFilesUpdated = 1}
    Just row -> do
      let dbMtime = frModTime row
      case dbMtime of
        Just dbMt
          | dbMt > mtime ->
              -- DB is newer — conflict
              pure
                emptySyncResult
                  { syncFilesProcessed = 1
                  , syncConflicts =
                      [ SyncConflict
                          { conflictFile = pathText
                          , conflictReason = "Database modification time is newer than file"
                          , conflictDbTime = Just dbMt
                          , conflictFileTime = mtime
                          }
                      ]
                  }
          | dbMt == mtime ->
              pure emptySyncResult{syncFilesProcessed = 1, syncFilesSkipped = 1}
        _ -> do
          _ <- storeOrgFile db org
          pure emptySyncResult{syncFilesProcessed = 1, syncFilesUpdated = 1}

mergeSyncResult :: SyncResult -> SyncResult -> SyncResult
mergeSyncResult a b =
  SyncResult
    { syncFilesProcessed = syncFilesProcessed a + syncFilesProcessed b
    , syncFilesSkipped = syncFilesSkipped a + syncFilesSkipped b
    , syncFilesUpdated = syncFilesUpdated a + syncFilesUpdated b
    , syncConflicts = syncConflicts a ++ syncConflicts b
    , syncErrors = syncErrors a ++ syncErrors b
    }

------------------------------------------------------------------------
-- Entry ID management
------------------------------------------------------------------------

{- | Ensure an entry has an ID property. Returns the (possibly modified)
entry and whether a new ID was generated.
-}
ensureEntryId :: Entry -> IO (Entry, Bool)
ensureEntryId ent =
  case ent ^? entryProperties . traverse . filtered (\p -> p ^. name == "ID") of
    Just _ -> pure (ent, False)
    Nothing -> do
      newId <- toText <$> nextRandom
      let idProp =
            Property
              { _propertyLoc = ent ^. entryLoc
              , _inherited = False
              , _name = "ID"
              , _value = T.unpack newId
              }
          ent' = ent & entryProperties %~ (idProp :)
      pure (ent', True)
