{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens hiding ((<.>))
import DB.Exec
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
import Org.Types (Collection (..), CollectionItem (..))
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

  paths <- maybe (pure []) getInputPaths (inputs opts)
  -- When linting with --check-dir, only the files changed since the last
  -- lint run are checked below, but the whole corpus still has to be parsed:
  -- the [[id:...]] resolution universe and cross-file duplicate detection
  -- need every file, not just the changed subset.
  checkPaths <- case command opts of
    Lint lintOpts -> winnowPaths (lintOpts ^. checkDir) paths
    _ -> pure paths
  coll <- readCollectionIO opts cfg paths

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
    Lint lintOpts ->
      execLint cfg lintOpts coll (filterOrgItems checkPaths coll)
    Stats statsOpts -> execStats cfg statsOpts coll
    Tags tagsOpts -> execTags cfg tagsOpts coll
    Trip tripOpts -> execTrip cfg tripOpts coll
    Db dbOpts -> execDb cfg dbOpts coll
    Site siteOpts -> execSite opts siteOpts coll
    Test -> case orgItems ^.. traverse . allEntries of
      [] -> pure ()
      e : _ -> do
        pPrint $ e ^? anyProperty cfg "ID"
        pPrint $ e ^? anyProperty cfg "CATEGORY"
        pPrint $ e ^? anyProperty cfg "TITLE"
        pPrint $ e ^? anyProperty cfg "ITEM"
        pPrint $ e ^? anyProperty cfg "FOOBAR"

-- Keep only the collection items whose file path is in the given set, but
-- preserve relative ordering.
filterOrgItems :: [FilePath] -> Collection -> Collection
filterOrgItems wanted (Collection xs) =
  Collection
    [ x
    | x <- xs
    , Just p <- [itemPath x]
    , p `elem` wanted
    ]
 where
  itemPath (OrgItem o) = Just (o ^. orgFilePath)
  itemPath (DataItem p) = Just p
