{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding ((<.>))
import Data.Foldable (foldl', forM_)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Map.Strict qualified as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import Options
import Org.Data
import Org.FileTags.Exec
import Org.JSON.Exec
import Org.Lint.Exec
import Org.Print
import Org.Read hiding (readFile)
import Org.Site.Exec
import Org.Types
import Text.Show.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  cfg <- configFromDotFile <$> readFile (opts ^. configFile)
  coll <- readCollectionIO cfg (opts ^. inputs)
  case opts ^. command of
    Parse -> do
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
    Json jsonOpts -> execJson cfg jsonOpts coll
    Print -> do
      let Config {..} = cfg
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
    Lint lintOpts -> execLint cfg lintOpts coll
    Tags tagsOpts -> execTags cfg tagsOpts coll
    Test -> do
      case coll ^.. items . traverse . _OrgItem . allEntries of
        [] -> pure ()
        e : _ -> do
          pPrint $ e ^? anyProperty cfg "ID"
          pPrint $ e ^? anyProperty cfg "CATEGORY"
          pPrint $ e ^? anyProperty cfg "TITLE"
          pPrint $ e ^? anyProperty cfg "ITEM"
          pPrint $ e ^? anyProperty cfg "FOOBAR"
    Site siteOpts -> execSite (opts ^. verbose) cfg siteOpts coll

configFromDotFile :: Text -> Config
configFromDotFile dot = Config {..}
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
