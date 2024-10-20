{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding ((<.>))
import Data.Foldable (forM_)
import Options
import Org.Data
import Org.JSON
import Org.Lint.Exec
import Org.Parse.Options
import Org.Print
import Org.Read
import Org.Site.Exec
import Org.Tags.Exec
import Org.Types
import System.FilePath
import Text.Show.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  coll <- readCollectionIO globalConfig (opts ^. inputs)
  case opts ^. command of
    Parse parseOpts -> do
      putStrLn $
        "There are a total of "
          ++ show (length (coll ^.. items . traverse . _OrgItem . allEntries))
          ++ " entries"
      pPrint $ countEntries coll $ \e m k ->
        k m $ case e ^. entryKeyword of
          Nothing -> "<plain>"
          Just (OpenKeyword _ kw) -> kw
          Just (ClosedKeyword _ kw) -> kw
      pPrint $ countEntries coll $ \e m k -> foldr (flip k) m (e ^. entryTags)
      forM_ (coll ^.. items . traverse . _OrgItem) $ \org -> do
        putStrLn $
          org ^. orgFilePath
            ++ ": "
            ++ show (length (org ^.. entries []))
            ++ " entries"
        forM_ (parseOpts ^. jsonDir) $ \dir ->
          orgFileToJSON
            (dir </> takeBaseName (org ^. orgFilePath) <.> "json")
            org
    Print -> do
      let Config {..} = globalConfig
      forM_ (coll ^.. items . traverse . _OrgItem) $ \org -> do
        forM_ (showOrgFile _propertyColumn _tagsColumn org) putStrLn
    Dump -> pPrint coll
    Outline ->
      mapM_
        (mapM_ putStrLn . concatMap summarizeEntry . _orgFileEntries)
        (coll ^.. items . traverse . _OrgItem)
    Stats ->
      mapM_
        (mapM_ putStrLn . concatMap summarizeEntry . _orgFileEntries)
        (coll ^.. items . traverse . _OrgItem)
    Lint lintOpts -> execLint globalConfig lintOpts coll
    Tags tagsOpts -> execTags globalConfig tagsOpts coll
    Test -> do
      case coll ^.. items . traverse . _OrgItem . allEntries of
        [] -> pure ()
        e : _ -> do
          pPrint $ e ^? anyProperty "ID"
          pPrint $ e ^? anyProperty "CATEGORY"
          pPrint $ e ^? anyProperty "TITLE"
          pPrint $ e ^? anyProperty "ITEM"
          pPrint $ e ^? anyProperty "FOOBAR"
    Site siteOpts -> execSite globalConfig siteOpts coll

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
        "TASK",
        "HABIT"
      ]
    _closedKeywords =
      [ "DONE",
        "FINISHED",
        "COMPLETE",
        "ABORTED",
        "CANCELED",
        "NOTE",
        "FEEDBACK",
        "LINK"
      ]
    _keywordTransitions =
      [ ( "TODO",
          ["DOING", "WAIT", "DEFER", "TASK", "HABIT", "CANCELED", "DONE"]
        ),
        ("PROJECT", ["ABORTED", "COMPLETE"]),
        ("DOING", ["TODO", "WAIT", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("WAIT", ["DOING", "TODO", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("DEFER", ["DOING", "WAIT", "TODO", "TASK", "CANCELED", "DONE"]),
        ( "TASK",
          ["DOING", "WAIT", "DEFER", "TODO", "HABIT", "CANCELED", "DONE"]
        ),
        ("HABIT", ["FINISHED"]),
        ("FINISHED", ["HABIT"]),
        ("DONE", ["TODO", "TASK", "HABIT"]),
        ("ABORTED", ["PROJECT"]),
        ("COMPLETE", ["PROJECT"]),
        ( "CANCELED",
          ["DOING", "WAIT", "DEFER", "TASK", "TODO", "HABIT", "DONE"]
        )
      ]
    _priorities =
      ["A", "B", "C"]
    _propertyColumn = 11
    _tagsColumn = 97
    _attachmentsDir = "/Users/johnw/org/data"
