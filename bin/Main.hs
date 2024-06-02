{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Except
import Data.Foldable (forM_)
import Data.List (permutations, subsequences)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options
import Org.Data
import Org.Lint
import Org.Printer
import Org.Types
import System.Directory
import System.Exit
import System.FilePath.Posix
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
    runExceptT (readOrgData globalConfig paths) >>= \case
      Left err -> do
        putStrLn $ "Cannot parse: " ++ err
        exitWith (ExitFailure 1)
      Right x -> pure x

  case opts ^. command of
    Parse _ -> doParse org
    TagsList _ -> doTagsList org
    CategoriesList _ -> doCategoriesList org
    Print _ -> doPrint org
    Dump _ -> doDump org
    Outline _ -> doOutline org
    Stats _ -> doStats org
    Lint level _ -> doLint level org
    Test _ -> doTest org
    TagTrees dryRun dir depth tagForUntagged _ ->
      doTagTrees dryRun dir depth tagForUntagged org

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

doParse :: OrgData -> IO ()
doParse org = do
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

doTagsList :: OrgData -> IO ()
doTagsList org = do
  let counts = countEntries
        org
        $ \e m k -> foldr (flip k) m (e ^. entryTags)
  forM_ (M.toList counts) $ \(tag, cnt) ->
    putStrLn $ show cnt ++ " " ++ T.unpack (tag ^. tagText)

doCategoriesList :: OrgData -> IO ()
doCategoriesList org = do
  let counts = countEntries org $ \e m k -> k m (e ^. entryCategory)
  forM_ (M.toList counts) $ \(cat, cnt) ->
    putStrLn $ show cnt ++ " " ++ T.unpack cat

doPrint :: OrgData -> IO ()
doPrint org =
  forM_ (org ^. orgFiles) $ \o ->
    T.putStrLn $ _OrgFile globalConfig (o ^. filePath) # o

doDump :: OrgData -> IO ()
doDump = pPrint

doOutline :: OrgData -> IO ()
doOutline org =
  mapM_
    (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
    (_orgFiles org)

doStats :: OrgData -> IO ()
doStats org =
  mapM_
    (mapM_ T.putStrLn . concatMap summarizeEntry . _fileEntries)
    (_orgFiles org)

doLint :: LintMessageKind -> OrgData -> IO ()
doLint level org = do
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
  case lintOrgData globalConfig level org of
    [] -> do
      putStrLn "Pass."
      exitSuccess
    xs -> do
      mapM_ (putStrLn . showLintOrg) xs
      exitWith (ExitFailure (length xs))

doTest :: OrgData -> IO ()
doTest org = do
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

doTagTrees ::
  Bool -> FilePath -> Int -> Maybe String -> OrgData -> IO ()
doTagTrees dryRun tagTreesDir depth tagForUntagged org = do
  isPresent <- doesDirectoryExist tagTreesDir
  if dryRun
    then when isPresent $ do
      putStrLn $ "Would remove existing " ++ tagTreesDir ++ " directory..."
    else when isPresent $ do
      putStrLn $ "Removing existing " ++ tagTreesDir ++ " directory..."
      removeDirectoryRecursive tagTreesDir

  forM_ (org ^.. orgFiles . traverse) $ \f -> do
    let path :: FilePath = f ^. filePath
        tags :: [Text] = f ^.. fileHeader . headerTags . traverse . _PlainTag
    forM_
      ( case (paths tags, tagForUntagged) of
          ([], Just tag) -> [[T.pack tag]]
          (xs, _) -> xs
      )
      $ \ts -> do
        let tagPath = T.unpack (T.intercalate "/" ts)
            tagDir = tagTreesDir </> tagPath
        if dryRun
          then
            putStrLn $
              takeFileName path
                ++ " -> "
                ++ tagPath
          else do
            createDirectoryIfMissing True tagDir
            createFileLink path (tagTreesDir </> tagPath </> takeFileName path)
  where
    paths =
      concatMap
        ( filter (\xs -> not (null xs) && length xs <= depth)
            . permutations
        )
        . subsequences
