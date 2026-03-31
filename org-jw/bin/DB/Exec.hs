{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Exec where

import Control.Lens
import DB.Options
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import Org.DB
import Org.Types

execDb :: Config -> DbOptions -> Collection -> IO ()
execDb _cfg opts coll = do
  let dbCfg = DBConfig (BS8.pack (opts ^. dbConnStr))
  case opts ^. dbCommand of
    DBInit -> withDB dbCfg $ \db -> do
      initDB db
      putStrLn "Database initialized."
    DBStore -> withDB dbCfg $ \db -> do
      initDB db
      storeCollection db coll
      putStrLn "Collection stored."
    DBQuery qopts -> withDB dbCfg $ \db -> do
      rows <- case qopts ^. queryOrgQl of
        Just ql -> do
          today <- utctDay <$> getCurrentTime
          queryByOrgQl db today (T.pack ql)
        Nothing -> case (qopts ^. queryKeyword, qopts ^. queryTag) of
          (Just kw, _) -> queryEntriesByKeyword db (T.pack kw)
          (_, Just tag) -> queryEntriesByTag db (T.pack tag)
          _ -> queryEntries db
      mapM_ printEntryRow rows

printEntryRow :: EntryRow -> IO ()
printEntryRow row =
  putStrLn $
    maybe "" T.unpack (erPath row)
      ++ ":"
      ++ show (erByteOffset row)
      ++ " "
      ++ maybe "" (\kw -> T.unpack kw ++ " ") (erKeywordValue row)
      ++ T.unpack (erTitle row)
