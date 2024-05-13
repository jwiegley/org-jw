{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad (void)
import Data.Map qualified as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Options qualified
import Org.Data
import Org.Expr
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
          ++ show (length (org ^.. allEntries []))
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
    Options.Lint -> do
      case org ^.. allEntries [] of
        [] -> pure ()
        e : _ -> do
          pPrint $ e ^? anyProperty "ID"
          pPrint $ e ^? anyProperty "CATEGORY"
          pPrint $ e ^? anyProperty "TITLE"
          pPrint $ e ^? anyProperty "ITEM"
          pPrint $ e ^? anyProperty "FOOBAR"
          pPrint $ expr "$TITLE"
          pPrint $ case expr "$TITLE" of
            Left _ -> pure ""
            Right x -> evalExpr x e :: Either String Text
      putStrLn $
        "Linting "
          ++ show (length (org ^.. allEntries []))
          ++ " entries ("
          ++ show
            ( length
                ( filter
                    (\e -> maybe False isTodo (e ^? keyword))
                    (org ^.. allEntries [])
                )
            )
          ++ " todo entries) across "
          ++ show (length (org ^. orgFiles))
          ++ " files"
      case lintOrgData org of
        [] -> do
          putStrLn "PASS"
          exitSuccess
        xs -> do
          mapM_ (putStrLn . showLintOrg) xs
          exitWith (ExitFailure (length xs))
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
