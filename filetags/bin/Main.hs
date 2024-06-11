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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
  paths <- case opts ^. command . commandInput of
    FileFromStdin -> pure ["<stdin>"]
    Paths paths -> pure paths
    ListFromStdin -> map T.unpack . T.lines . T.decodeUtf8 <$> readStdin
    FilesFromFile path -> readLines path
  cs <-
    runExceptT (readCollection globalConfig paths) >>= \case
      Left msg -> do
        putStrLn $ "Cannot parse: " ++ msg
        exitWith (ExitFailure 1)
      Right x -> pure x
  case opts ^. command of
    TagsList _ -> doTagsList cs
    TagTrees dryRun dir overwrite depth tagForUntagged _ ->
      makeTagTrees dryRun dir overwrite depth (PlainTag <$> tagForUntagged) cs
    Filter dryRun dir overwrite expr _ ->
      makeFilter dryRun dir overwrite expr cs

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
        "DELEGATED",
        "APPT"
      ]
    _closedKeywords =
      [ "DONE",
        "CANCELED",
        "NOTE",
        "LINK"
      ]
    _keywordTransitions =
      [ ("TODO", ["DOING", "WAIT", "DEFER", "DELEGATED", "CANCELED", "DONE"]),
        ("PROJECT", ["CANCELED", "DONE"]),
        ("DOING", ["TODO", "WAIT", "DEFER", "DELEGATED", "CANCELED", "DONE"]),
        ("WAIT", ["DOING", "TODO", "DEFER", "DELEGATED", "CANCELED", "DONE"]),
        ("DEFER", ["DOING", "WAIT", "TODO", "DELEGATED", "CANCELED", "DONE"]),
        ("DELEGATED", ["DOING", "WAIT", "DEFER", "TODO", "CANCELED", "DONE"]),
        ("APPT", ["DOING", "CANCELED", "DONE"]),
        ("DONE", ["TODO"]),
        ("CANCELED", ["DOING", "WAIT", "DEFER", "DELEGATED", "TODO", "DONE"])
      ]
    _priorities =
      ["A", "B", "C"]
    _propertyColumn = 11
    _tagsColumn = 97

doTagsList :: Collection -> IO ()
doTagsList cs = do
  let counts = countEntries cs $ \e m k -> foldr (flip k) m (e ^. entryTags)
  forM_ (M.toList counts) $ \(tag, cnt) ->
    putStrLn $ show cnt ++ " " ++ tag ^. tagString
