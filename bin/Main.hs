{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens
import Control.Monad.Except
import Data.Foldable (forM_)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options
import Org.Data
import Org.Lint
import Org.Printer
import Org.Types
import System.Exit
import Text.Show.Pretty
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions

  paths <- case opts ^. command . commandInput of
    FileFromStdin ->
      pure ["<stdin>"]
    Paths paths ->
      pure paths
    ListFromStdin ->
      map T.unpack . T.lines <$> readStdin
    FilesFromFile path ->
      map T.unpack <$> readLines path

  org <-
    runExceptT (readOrgData Config {..} paths) >>= \case
      Left err -> do
        putStrLn $ "Cannot parse: " ++ err
        exitWith (ExitFailure 1)
      Right x -> pure x

  case opts ^. command of
    Parse _ -> do
      putStrLn $
        "There are a total of "
          ++ show (length (org ^.. orgFiles . traverse . allEntries))
          ++ " entries"
      pPrint $ countEntries org $ \e m k ->
        k m $ case e ^. entryKeyword of
          Nothing -> "<plain>"
          Just (OpenKeyword _ kw) -> kw
          Just (ClosedKeyword _ kw) -> kw
      pPrint $ countEntries org $ \e m k -> foldr (flip k) m (e ^. entryTags)
      forM_ (org ^. orgFiles) $ \o ->
        putStrLn $
          o ^. filePath
            ++ ": "
            ++ show (length (o ^.. entries []))
            ++ " entries"
    TagsList _ -> do
      let counts = countEntries
            org
            $ \e m k -> foldr (flip k) m (e ^. entryTags)
      forM_ (M.toList counts) $ \(tag, cnt) ->
        putStrLn $ show cnt ++ " " ++ T.unpack (tag ^. tagText)
    CategoriesList _ -> do
      let counts = countEntries org $ \e m k -> k m (e ^. entryCategory)
      forM_ (M.toList counts) $ \(cat, cnt) ->
        putStrLn $ show cnt ++ " " ++ T.unpack cat
    Print _ ->
      forM_ (org ^. orgFiles) $ \o ->
        T.putStrLn $ _OrgFile Config {..} (o ^. filePath) # o
    Dump _ -> pPrint org
    Outline _ ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
    Stats _ ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
    Lint level _ -> do
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
      case lintOrgData Config {..} level org of
        [] -> do
          putStrLn "Pass."
          exitSuccess
        xs -> do
          mapM_ (putStrLn . showLintOrg) xs
          exitWith (ExitFailure (length xs))
    Test _ -> do
      forM_ (org ^. pre (orgFiles . traverse)) $ \f -> do
        putStrLn "filePath:"
        pPrint $ f ^. filePath
        putStrLn "fileTitle:"
        pPrint $ f ^. fileTitle
        putStrLn "fileTimestamp:"
        pPrint $ f ^? fileTimestamp . to showTime
        putStrLn "fileCreatedTime:"
        pPrint $ f ^? fileCreatedTime . to showTime
      forM_ (org ^. pre (orgFiles . traverse . allEntries)) $ \e -> do
        putStrLn "entry ID:"
        pPrint $ e ^? anyProperty "ID"
        putStrLn "entry CATEGORY:"
        pPrint $ e ^? anyProperty "CATEGORY"
        putStrLn "entry TITLE:"
        pPrint $ e ^? anyProperty "TITLE"
        putStrLn "entry ITEM:"
        pPrint $ e ^? anyProperty "ITEM"
        putStrLn "entry FOOBAR:"
        pPrint $ e ^? anyProperty "FOOBAR"
        putStrLn $ "Entry text: " ++ ppShow (e ^. entryText)
        putStrLn $ "Lead space: " ++ ppShow (e ^. entryText . leadSpace)
        putStrLn $ " End space: " ++ ppShow (e ^. entryText . endSpace)
        let e' = e & entryText . endSpace .~ ""
        putStrLn $ "Entry text': " ++ ppShow (e' ^. entryText)
        putStrLn $ "State history': " ++ ppShow (e' ^.. entryStateHistory)
        putStrLn "Entire entry:"
        pPrint e
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
