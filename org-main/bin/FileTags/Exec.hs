{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileTags.Exec where

import Control.Lens hiding (List)
import Data.Foldable (forM_)
import Data.Map qualified as M
import FileTags.Options
import Org.Data
import Org.FileTags.Filter
import Org.FileTags.TagTrees
import Org.Types
import Prelude hiding (readFile)

execTags :: Config -> TagsOptions -> Collection -> IO ()
execTags _cfg opts coll = case opts ^. command of
  List -> do
    let counts = countEntries coll $ \e m k -> foldr (flip k) m (e ^. entryTags)
    forM_ (M.toList counts) $ \(tag, cnt) ->
      putStrLn $ show cnt ++ " " ++ tag ^. tagString
  TagTrees dryRun dir overwrite depth tagForUntagged ->
    makeTagTrees
      dryRun
      dir
      overwrite
      depth
      (tagForUntagged)
      (collectionPaths coll)
  Filter dryRun dir overwrite expr ->
    makeFilter dryRun dir overwrite expr (collectionPaths coll)
