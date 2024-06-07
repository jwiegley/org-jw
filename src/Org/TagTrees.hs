module Org.TagTrees where

import Control.Arrow ((***))
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Foldable (forM_)
import Data.List (permutations, subsequences)
import Data.Maybe (maybeToList)
import Org.Data
import Org.Types
import System.Directory
import System.Exit
import System.FilePath.Posix

combinations :: [a] -> [[a]]
combinations = concatMap (filter (not . null) . permutations) . subsequences

tagPath :: [Tag] -> FilePath
tagPath = foldr ((</>) . (^. _PlainTag)) ""

createEmptyDirectory :: Bool -> FilePath -> IO ()
createEmptyDirectory overwrite dir = do
  isPresent <- doesDirectoryExist dir
  if isPresent
    then
      if overwrite
        then do
          removeDirectoryRecursive dir
          createDirectoryIfMissing True dir
        else do
          contents <- listDirectory dir
          unless (null contents) $ do
            putStrLn $ "Cannot overwrite directory " ++ dir
            exitWith (ExitFailure 1)
    else createDirectoryIfMissing True dir

createLinkInDirectory :: FilePath -> FilePath -> IO ()
createLinkInDirectory src dir = do
  createDirectoryIfMissing True dir
  createFileLink src (dir </> takeFileName src)

makeTagTrees ::
  Bool -> FilePath -> Bool -> Int -> Maybe Tag -> Collection -> IO ()
makeTagTrees dryRun tagTreesDir overwrite depth tagForUntagged cs = do
  unless dryRun $
    createEmptyDirectory overwrite tagTreesDir

  (count, maxDepth) <- flip execStateT (0 :: Int, 0 :: Int) $
    forM_ (cs ^. items) $ \f -> do
      let tags = case f ^.. fileTags . traverse of
            [] -> maybeToList tagForUntagged
            xs -> xs
      forM_ (filter (\ts -> length ts <= depth) (combinations tags)) $
        \tagSet -> do
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
