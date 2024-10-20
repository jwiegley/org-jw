{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Data (Data)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (forM)
import Data.Typeable (Typeable)
import FlatParse.Combinators
import FlatParse.Stateful qualified as FP
import GHC.Generics
import Org.Parse
import Org.Types
import System.FilePath.Posix
import System.IO

data InputFiles
  = FileFromStdin -- '-f -'
  | ListFromStdin -- '-F -'
  | Paths [FilePath] -- '<path>...'
  | FilesFromFile FilePath -- '-F <path>'
  deriving (Data, Show, Eq, Typeable, Generic, Ord)

readOrgFile ::
  (MonadError (Loc, String) m, MonadIO m) =>
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
  (MonadError (Loc, String) m, MonadIO m) =>
  Config ->
  FilePath ->
  m CollectionItem
readCollectionItem cfg path = do
  if takeExtension path == ".org"
    then OrgItem <$> readOrgFile cfg path
    else pure $ DataItem path

foldCollection ::
  (MonadError (Loc, String) m, MonadIO m) =>
  Config ->
  InputFiles ->
  a ->
  (FilePath -> Either (Loc, String) CollectionItem -> a -> m a) ->
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
  (MonadError (Loc, String) m, MonadIO m) =>
  Config ->
  InputFiles ->
  m Collection
readCollection cfg inputs =
  Collection <$> foldCollection cfg inputs [] go
  where
    go _path (Left err) _ = throwError err
    go _path (Right x) acc = pure (x : acc)

mapCollection ::
  Config ->
  InputFiles ->
  IO [IO (Either (Loc, String) CollectionItem)]
mapCollection cfg inputs = do
  paths <- case inputs of
    FileFromStdin -> pure ["<stdin>"]
    Paths paths -> pure paths
    ListFromStdin -> map T.unpack . T.lines . T.decodeUtf8 <$> readStdin
    FilesFromFile path -> readLines path
  forM paths $ \path ->
    pure $ do
      eres <- runExceptT (tryError (readCollectionItem cfg path))
      pure $ case eres of
        Left err -> Left err
        Right (Left err) -> Left err
        Right (Right x) -> Right x

readCollectionIO ::
  Config ->
  InputFiles ->
  IO Collection
readCollectionIO cfg inputs = do
  actions <- mapCollection cfg inputs
  coll <- PIO.parallelInterleaved actions
  PIO.stopGlobalPool
  fmap (Collection . concat) $ forM coll $ \case
    Left (loc, err) -> do
      contents <- B.readFile (_file loc)
      case FP.posLineCols contents [FP.Pos (_pos loc)] of
        [(line, col)] -> do
          putStrLn $
            _file loc
              ++ ":"
              ++ show (succ line)
              ++ ":"
              ++ show col
              ++ ": PARSE ERROR: "
              ++ err
          pure []
        _ -> error "impossible"
    Right x -> pure [x]
