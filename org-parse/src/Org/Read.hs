{-# LANGUAGE ImportQualifiedPost #-}

module Org.Read where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Org.Parse
import Org.Parse.Combinators
import Org.Types
import System.FilePath.Posix
import System.IO

readOrgFile :: (MonadIO m) => Config -> FilePath -> ExceptT String m OrgFile
readOrgFile cfg path = do
  org <-
    liftIO $
      withFile path ReadMode $
        fmap (readOrgFile_ cfg path) . B.hGetContents
  liftResult org

readStdin :: (MonadIO m) => m ByteString
readStdin = liftIO B.getContents

readFile :: (MonadIO m) => FilePath -> m ByteString
readFile path = liftIO (B.readFile path)

readLines :: (MonadIO m) => FilePath -> m [String]
readLines path = lines <$> liftIO (System.IO.readFile path)

readCollectionItem ::
  (MonadIO m) =>
  Config ->
  FilePath ->
  ExceptT String m CollectionItem
readCollectionItem cfg path = do
  if takeExtension path == ".org"
    then OrgItem <$> readOrgFile cfg path
    else pure $ DataItem path

readCollection ::
  (MonadIO m) =>
  Config ->
  [FilePath] ->
  ExceptT String m Collection
readCollection cfg paths = Collection <$> mapM (readCollectionItem cfg) paths
