{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad (void)
import Data.Map qualified as M
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Options qualified
import Org.Data
import Org.Lint
import Org.Printer
import Org.Types
import System.Exit
import Text.Show.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- Options.getOptions

  texts <- case opts ^. Options.paths of
    Options.FileFromStdin ->
      (: []) . ("<stdin>",) <$> readStdin
    Options.SingleFile path ->
      (: []) . (path,) <$> readFile path
    Options.ListFromStdin ->
      (mapM ((\p -> (p,) <$> readFile p) . T.unpack) . T.lines) =<< readStdin
    Options.FilesFromFile path ->
      mapM ((\p -> (p,) <$> readFile p) . T.unpack) =<< readLines path

  let org = case readOrgData Config {..} texts of
        Left err ->
          error $ "Cannot parse: " ++ err
        Right x -> x

  case opts ^. Options.command of
    Options.Parse -> do
      putStrLn $
        "There are a total of "
          ++ show (length (org ^.. orgFiles . traverse . allEntries))
          ++ " entries"
      pPrint $ countEntries org $ \e m k ->
        k m $ case e ^. entryKeyword of
          Nothing -> "<plain>"
          Just (OpenKeyword kw) -> kw
          Just (ClosedKeyword kw) -> kw
      pPrint $ countEntries org $ \e m k -> foldr (flip k) m (e ^. entryTags)
      void $ flip M.traverseWithKey (org ^. orgFiles) $ \path o -> do
        putStrLn $
          path ++ ": " ++ show (length (o ^.. entries [])) ++ " entries"
    Options.Print ->
      void $ flip M.traverseWithKey (org ^. orgFiles) $ \path o ->
        T.putStrLn $ _OrgFile Config {..} path # o
    Options.Dump -> pPrint org
    Options.Outline ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
    Options.Stats ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
    Options.Lint level -> do
      putStrLn $
        "Linting "
          ++ show (length (org ^.. orgFiles . traverse . allEntries))
          ++ " entries ("
          ++ show
            ( length
                ( filter
                    (\e -> maybe False isTodo (e ^? keyword))
                    (org ^.. orgFiles . traverse . allEntries)
                )
            )
          ++ " todo entries) across "
          ++ show (length (org ^. orgFiles))
          ++ " files"
      case lintOrgData level org of
        [] -> do
          putStrLn "Pass."
          exitSuccess
        xs -> do
          mapM_ (putStrLn . showLintOrg) xs
          exitWith (ExitFailure (length xs))
    Options.Test -> do
      case org ^.. orgFiles . traverse . allEntries of
        [] -> pure ()
        e : _ -> do
          pPrint $ e ^? anyProperty "ID"
          pPrint $ e ^? anyProperty "CATEGORY"
          pPrint $ e ^? anyProperty "TITLE"
          pPrint $ e ^? anyProperty "ITEM"
          pPrint $ e ^? anyProperty "FOOBAR"
          putStrLn $ "Entry text: " ++ ppShow (e ^. entryText)
          putStrLn $ "Lead space: " ++ ppShow (e ^. entryText . leadSpace)
          putStrLn $ " End space: " ++ ppShow (e ^. entryText . endSpace)
          let e' = e & entryText . endSpace .~ ""
          putStrLn $ "Entry text': " ++ ppShow (e' ^. entryText)
  where
    -- jww (2024-05-10): These details need to be read from a file, or from
    -- command-line options.
    _openKeywords =
      [ "TODO",
        "CATEGORY",
        "PROJECT",
        "STARTED",
        "WAITING",
        "DEFERRED",
        "SOMEDAY",
        "DELEGATED",
        "APPT"
      ]
    _closedKeywords =
      [ "DONE",
        "CANCELED",
        "NOTE",
        "LINK"
      ]
    _priorities =
      ["A", "B", "C"]
    _specialTags =
      ["ARCHIVE", "FILE", "URL"]
    _propertyColumn = 11
    _tagsColumn = 97
