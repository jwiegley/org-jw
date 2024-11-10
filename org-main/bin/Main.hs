{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding ((<.>))
import Data.Foldable (forM_)
import Data.Text.Lazy.IO (readFile)
import Data.Yaml qualified as Yaml
import FileTags.Exec
import JSON.Exec
import Lint.Exec
import Lint.Options
import Options
import Org.Data
import Org.Print
import Read hiding (readFile)
import Site.Exec
import Stats.Exec
import System.Exit
import Text.Show.Pretty
import Trip.Exec
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions

  cfg' <-
    Yaml.decodeFileEither (configFile opts) >>= \case
      Left err -> do
        putStrLn $
          "Could not open or parse config file "
            ++ configFile opts
            ++ ": "
            ++ show err
        exitFailure
      Right conf -> pure conf
  cfg <- case keywordsGraph opts of
    Nothing -> pure cfg'
    Just path -> applyDotFile cfg' <$> readFile path

  paths <- getInputPaths (inputs opts)
  paths' <- case command opts of
    Lint lintOpts ->
      -- When linting, only check files that have changed since the last lint
      -- run, if --check-dir has been given.
      winnowPaths (lintOpts ^. checkDir) paths
    _ -> pure paths
  coll <- readCollectionIO opts cfg paths'

  let orgItems = coll ^.. items . traverse . _OrgItem
  case command opts of
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
    Site siteOpts -> execSite opts siteOpts coll
    Test -> case orgItems ^.. traverse . allEntries of
      [] -> pure ()
      e : _ -> do
        pPrint $ e ^? anyProperty cfg "ID"
        pPrint $ e ^? anyProperty cfg "CATEGORY"
        pPrint $ e ^? anyProperty cfg "TITLE"
        pPrint $ e ^? anyProperty cfg "ITEM"
        pPrint $ e ^? anyProperty cfg "FOOBAR"
