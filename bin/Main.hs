{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Control.Monad (void)
import Data.Map qualified as M
-- import Data.Set qualified as S
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Options qualified
import Org.Data
import Org.Printer
import Org.Types
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
          ++ show (length (org ^.. allEntries []))
          ++ " entries"
      let keywordMap =
            foldr
              ( \e ->
                  let kw = case e ^. entryKeyword of
                        Nothing -> "<plain>"
                        Just (OpenKeyword k) -> k
                        Just (ClosedKeyword k) -> k
                   in at kw %~ Just . maybe (0 :: Int) succ
              )
              M.empty
              (org ^.. allEntries [])
      pPrint keywordMap
      void $ flip M.traverseWithKey (org ^. orgFiles) $ \path o -> do
        putStrLn $
          path ++ ": " ++ show (length (o ^.. entries [])) ++ " entries"
    {-
            putStrLn "Tags found:"
            mapM_
              ( \case
                  PlainTag tag -> T.putStrLn $ "  " <> tag
                  SpecialTag tag -> T.putStrLn $ "  [" <> tag <> "]"
              )
              ( foldr
                  S.insert
                  mempty
                  (concatMap _entryTags (_fileEntries o))
              )
            putStrLn "Contexts found:"
            mapM_
              ( \case
                  Nothing -> pure ()
                  Just x -> T.putStrLn $ "  " <> x
              )
              (foldr (S.insert . _entryContext) mempty (_fileEntries o))
            putStrLn "Locators found:"
            mapM_
              ( \case
                  Nothing -> pure ()
                  Just x -> T.putStrLn $ "  " <> x
              )
              (foldr (S.insert . _entryLocator) mempty (_fileEntries o))
            mapM_
              ( \title -> case parseMaybe
                  (count 4 upperChar :: BasicParser String)
                  title of
                  Nothing -> pure ()
                  Just _ -> T.putStrLn $ "Fishy title: " <> title
              )
              (foldr (S.insert . _entryTitle) mempty (_fileEntries o))
    -}
    Options.Print ->
      void $ flip M.traverseWithKey (org ^. orgFiles) $ \path o ->
        T.putStrLn $ _orgFile Config {..} path # o
    Options.Dump -> do
      pPrint org
      pPrint $ entriesMap [] org
    Options.Outline ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
    Options.Stats ->
      mapM_
        (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
        (_orgFiles org)
  where
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
