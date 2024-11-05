{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stats.Exec where

import Control.Lens hiding ((<.>))
import Data.Foldable (forM_)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Org.Data
import Org.Types
import Stats.Options
import System.IO
import Prelude hiding (readFile)

fsize :: FilePath -> IO Integer
fsize path = withFile path ReadMode hFileSize

execStats :: Config -> StatsOptions -> Collection -> IO ()
execStats cfg _opts coll = do
  putStrLn $ show (length orgItems) ++ " files"

  totalSize <- sum <$> mapM (fsize . view orgFilePath) orgItems
  putStrLn $ show (fromIntegral totalSize / 1024.0 / 1024.0 :: Double) ++ " MB"

  putStrLn $ show (length orgEntries) ++ " entries"
  putStrLn $ show (length orgTodos) ++ " items"
  putStrLn $ show (length orgOpenTodos) ++ " open items"
  putStrLn $ show (length orgTodos - length orgOpenTodos) ++ " closed items"

  putStrLn "\nKeywords:"
  forM_ (reverse (sortOn snd (M.assocs allKeywords))) $ \(x, n) ->
    putStrLn $ "  " ++ show n ++ " " ++ x

  putStrLn "\nTags:"
  forM_ (reverse (sortOn snd (M.assocs allTags))) $ \(PlainTag x, n) ->
    putStrLn $ "  " ++ show n ++ " " ++ x
  where
    orgItems = coll ^.. items . traverse . _OrgItem
    orgEntries = orgItems ^.. traverse . allEntries
    orgTodos = orgEntries ^.. traverse . keyword . filtered (isTodo cfg)
    orgOpenTodos = orgTodos ^.. traverse . filtered (isOpenTodo cfg)

    allKeywords = countEntries coll $ \e m k ->
      k m $ case e ^. entryKeyword of
        Nothing -> "<plain>"
        Just (OpenKeyword _ kw) -> kw
        Just (ClosedKeyword _ kw) -> kw

    allTags = countEntries coll $ \e m k ->
      foldr (flip k) m (e ^. entryTags)
