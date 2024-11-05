{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding ((<.>))
import Data.Foldable (forM_)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Map.Strict qualified as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import FileTags.Exec
import JSON.Exec
import Lint.Exec
import Lint.Options
import Options
import Org.Data
import Org.Print
import Org.Types
import Read hiding (readFile)
import Site.Exec
import Stats.Exec
import Text.Show.Pretty
import Trip.Exec
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  cfg <-
    configFromDotFile (opts ^. Options.cacheDir)
      <$> readFile (opts ^. configFile)
  paths <- getInputPaths (opts ^. inputs)
  paths' <- case opts ^. command of
    Lint lintOpts ->
      -- When linting, only check files that have changed since the last lint
      -- run, if --check-dir has been given.
      winnowPaths (lintOpts ^. checkDir) paths
    _ -> pure paths
  coll <- readCollectionIO cfg paths'
  let orgItems = coll ^.. items . traverse . _OrgItem
  case opts ^. command of
    Parse ->
      putStrLn $ "Parsed " ++ show (length orgItems) ++ " Org-mode files"
    Print -> forM_ orgItems $ \org ->
      forM_ (showOrgFile cfg org) putStrLn
    Dump -> pPrint coll
    Outline ->
      forM_ orgItems $ \org ->
        forM_ (org ^. orgFileEntries) $
          mapM_ putStrLn . summarizeEntry cfg
    Json jsonOpts -> execJson cfg jsonOpts coll
    Lint lintOpts -> execLint cfg lintOpts coll
    Stats statsOpts -> execStats cfg statsOpts coll
    Tags tagsOpts -> execTags cfg tagsOpts coll
    Trip tripOpts -> execTrip cfg tripOpts coll
    Site siteOpts -> execSite (opts ^. verbose) cfg siteOpts coll
    Test -> case orgItems ^.. traverse . allEntries of
      [] -> pure ()
      e : _ -> do
        pPrint $ e ^? anyProperty cfg "ID"
        pPrint $ e ^? anyProperty cfg "CATEGORY"
        pPrint $ e ^? anyProperty cfg "TITLE"
        pPrint $ e ^? anyProperty cfg "ITEM"
        pPrint $ e ^? anyProperty cfg "FOOBAR"

configFromDotFile :: Maybe FilePath -> Text -> Config
configFromDotFile cdir dot = Config {..}
  where
    gr :: DotGraph String
    gr = parseDotGraph dot

    _keywordTransitions =
      M.toList $
        foldl'
          ( \m e ->
              m
                & at (fromNode e) %~ \case
                  Nothing -> Just [toNode e]
                  Just ns -> Just (toNode e : ns)
          )
          mempty
          (graphEdges gr)

    _startKeywords = nodesWithColor Red
    _openKeywords = _startKeywords ++ nodesWithColor Blue
    _closedKeywords = nodesWithColor Green
    _priorities = ["A", "B", "C"]
    _propertyColumn = 11
    _tagsColumn = 97
    _attachmentsDir = "/Users/johnw/org/data"
    _cacheDir = cdir
    _checkDir = Nothing

    nodesWithColor :: X11Color -> [String]
    nodesWithColor clr = map nodeID (filter (hasColor clr) (graphNodes gr))

    hasColor :: X11Color -> DotNode String -> Bool
    hasColor clr node =
      any
        ( \attr -> case attr of
            Color cs ->
              any
                ( \c -> case wColor c of
                    X11Color x11 -> x11 == clr
                    _ -> False
                )
                cs
            _ -> False
        )
        (nodeAttributes node)
