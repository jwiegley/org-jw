{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad.Except
import Data.Foldable (forM_)
import Data.Map qualified as M
import Options
import Org.Data
import Org.Filter
import Org.Read
import Org.TagTrees
import Org.Types
import System.Exit
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  cs <-
    runExceptT
      ( readCollection
          globalConfig
          (opts ^. command . commandInput)
      )
      >>= \case
        Left (_, msg) -> do
          putStrLn $ "Error: " ++ msg
          exitWith (ExitFailure 1)
        Right x -> pure x
  case opts ^. command of
    TagsList _ -> doTagsList cs
    TagTrees dryRun dir overwrite depth tagForUntagged _ ->
      makeTagTrees
        dryRun
        dir
        overwrite
        depth
        (tagForUntagged)
        (collectionPaths cs)
    Filter dryRun dir overwrite expr _ ->
      makeFilter dryRun dir overwrite expr (collectionPaths cs)

globalConfig :: Config
globalConfig = Config {..}
  where
    -- jww (2024-05-10): These details need to be read from a file, or from
    -- command-line options.
    _openKeywords =
      [ "TODO",
        "PROJECT",
        "DOING",
        "WAIT",
        "DEFER",
        "TASK"
      ]
    _closedKeywords =
      [ "DONE",
        "COMPLETE",
        "ABORTED",
        "CANCELED",
        "NOTE",
        "FEEDBACK",
        "LINK"
      ]
    _keywordTransitions =
      [ ("TODO", ["DOING", "WAIT", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("PROJECT", ["ABORTED", "COMPLETE"]),
        ("DOING", ["TODO", "WAIT", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("WAIT", ["DOING", "TODO", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("DEFER", ["DOING", "WAIT", "TODO", "TASK", "CANCELED", "DONE"]),
        ("TASK", ["DOING", "WAIT", "DEFER", "TODO", "CANCELED", "DONE"]),
        ("DONE", ["TODO", "TASK"]),
        ("ABORTED", ["PROJECT"]),
        ("COMPLETE", ["PROJECT"]),
        ("CANCELED", ["DOING", "WAIT", "DEFER", "TASK", "TODO", "DONE"])
      ]
    _priorities =
      ["A", "B", "C"]
    _propertyColumn = 11
    _tagsColumn = 97
    _attachmentsDir = "/Users/johnw/org/data"

doTagsList :: Collection -> IO ()
doTagsList cs = do
  let counts = countEntries cs $ \e m k -> foldr (flip k) m (e ^. entryTags)
  forM_ (M.toList counts) $ \(tag, cnt) ->
    putStrLn $ show cnt ++ " " ++ tag ^. tagString
