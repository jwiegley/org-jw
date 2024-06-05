{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Org.TagTrees (makeTagTrees) where

import Control.Arrow ((***))
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Foldable (forM_)
import Data.List (permutations, subsequences)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text.Lens
import Data.Traversable (forM)
import Org.Data
import Org.Types
import System.Directory
import System.Exit
import System.FilePath.Posix
import Prelude hiding (readFile)

combinations :: [a] -> [[a]]
combinations = concatMap (filter (not . null) . permutations) . subsequences

tagPath :: [Tag] -> FilePath
tagPath = foldr ((</>) . (^. _PlainTag . unpacked)) ""

createEmptyDirectory :: FilePath -> IO ()
createEmptyDirectory dir = do
  isPresent <- doesDirectoryExist dir
  if isPresent
    then do
      contents <- listDirectory dir
      unless (Prelude.null contents) $ do
        putStrLn $ "Cannot overwrite directory " ++ dir
        exitWith (ExitFailure 1)
    else createDirectoryIfMissing True dir

createLinkInDirectory :: FilePath -> FilePath -> IO ()
createLinkInDirectory src dir = do
  createDirectoryIfMissing True dir
  createFileLink src (dir </> takeFileName src)

data DirEntry
  = DirEntry FilePath
  | FileEntry FilePath
  deriving (Eq, Ord, Show)

listDirectoryRecursive :: FilePath -> IO [DirEntry]
listDirectoryRecursive dir = do
  es <- listDirectory dir
  fmap concat $ forM es $ \e -> do
    let path = dir </> e
    isDir <- doesDirectoryExist path
    if isDir
      then (DirEntry path :) <$> listDirectoryRecursive path
      else pure [FileEntry path]

updateLinkInDirectory :: FilePath -> FilePath -> StateT (Set DirEntry) IO ()
updateLinkInDirectory src dir = do
  es <- get
  if FileEntry dest `S.member` es
    then modify (FileEntry dir `S.delete`)
    else liftIO $ do
      unless (DirEntry dir `S.member` es) $
        createDirectoryIfMissing True dir
      createFileLink src dest
  where
    dest = dir </> takeFileName src

makeTagTrees ::
  Bool -> FilePath -> Int -> Maybe Tag -> Collection -> IO ()
makeTagTrees dryRun tagTreesDir depth tagForUntagged cs = do
  unless dryRun $
    createEmptyDirectory tagTreesDir

  (count, maxDepth) <- flip execStateT (0 :: Int, 0 :: Int) $
    forM_ (cs ^. items) $ \f -> do
      let tags = case f ^.. fileTags . traverse of
            [] -> maybeToList tagForUntagged
            xs -> xs
          tagSets = filter (\ts -> length ts <= depth) (combinations tags)
      forM_ tagSets $ \tagSet -> do
        modify (succ *** max (length tagSet))
        unless dryRun $
          liftIO $
            createLinkInDirectory
              (f ^. filePath)
              (tagTreesDir </> tagPath tagSet)

  putStrLn $
    (if dryRun then "Would create " else "Created ")
      ++ show count
      ++ " symbolic links, at a maximum depth of "
      ++ show maxDepth
      ++ " links"
