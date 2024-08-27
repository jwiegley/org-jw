{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Org.Read where

import Control.Concurrent.ParallelIO qualified as PIO
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (forM)
import FlatParse.Combinators
import Org.Parse
import Org.Types
import System.FilePath.Posix
import System.IO

data InputFiles
  = FileFromStdin -- '-f -'
  | ListFromStdin -- '-F -'
  | Paths [FilePath] -- '<path>...'
  | FilesFromFile FilePath -- '-F <path>'
  deriving (Show, Eq)

readOrgFile ::
  (MonadError String m, MonadIO m) =>
  Config ->
  FilePath ->
  m OrgFile
readOrgFile cfg path = do
  res <-
    liftIO $
      withFile path ReadMode $
        fmap (readOrgFile_ cfg path) . B.hGetContents
  liftResult path res

readStdin :: (MonadIO m) => m ByteString
readStdin = liftIO B.getContents

readFile :: (MonadIO m) => FilePath -> m ByteString
readFile path = liftIO (B.readFile path)

readLines :: (MonadIO m) => FilePath -> m [String]
readLines path = lines <$> liftIO (System.IO.readFile path)

readCollectionItem ::
  (MonadError String m, MonadIO m) =>
  Config ->
  FilePath ->
  m CollectionItem
readCollectionItem cfg path = do
  if takeExtension path == ".org"
    then OrgItem <$> readOrgFile cfg path
    else pure $ DataItem path

foldCollection ::
  (MonadError String m, MonadIO m) =>
  Config ->
  InputFiles ->
  a ->
  (FilePath -> Either String CollectionItem -> a -> m a) ->
  m a
foldCollection cfg inputs z f = do
  paths <- case inputs of
    FileFromStdin -> pure ["<stdin>"]
    Paths paths -> pure paths
    ListFromStdin -> map T.unpack . T.lines . T.decodeUtf8 <$> readStdin
    FilesFromFile path -> readLines path
  (\k -> foldM k z paths) $ \acc path ->
    tryError (readCollectionItem cfg path) >>= \case
      Left e -> f path (Left e) acc
      Right x -> f path (Right x) acc

readCollection ::
  (MonadError String m, MonadIO m) =>
  Config ->
  InputFiles ->
  m Collection
readCollection cfg inputs = Collection <$> foldCollection cfg inputs [] go
  where
    go _path (Left err) _ = throwError err
    go _path (Right x) acc = pure (x : acc)

mapCollection ::
  Config ->
  InputFiles ->
  IO [IO CollectionItem]
mapCollection cfg inputs = do
  paths <- case inputs of
    FileFromStdin -> pure ["<stdin>"]
    Paths paths -> pure paths
    ListFromStdin -> map T.unpack . T.lines . T.decodeUtf8 <$> readStdin
    FilesFromFile path -> readLines path
  forM paths $ \path ->
    pure $ do
      eres <- runExceptT (tryError (readCollectionItem cfg path))
      case eres of
        Left err -> error err
        Right (Left err) -> error err
        Right (Right x) -> pure x

readCollectionIO ::
  Config ->
  InputFiles ->
  IO Collection
readCollectionIO cfg inputs = do
  actions <- mapCollection cfg inputs
  coll <- Collection <$> PIO.parallelInterleaved actions
  PIO.stopGlobalPool
  pure coll
