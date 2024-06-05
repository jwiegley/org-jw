{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Org.TagTrees
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
  cs <-
    runExceptT (readCollection globalConfig paths) >>= \case
      Left err -> do
        putStrLn $ "Cannot parse: " ++ err
        exitWith (ExitFailure 1)
      Right x -> pure x
  case opts ^. command of
    Parse _ -> doParse cs
    TagsList _ -> doTagsList cs
    CategoriesList _ -> doCategoriesList cs
    Print _ -> doPrint cs
    Dump _ -> doDump cs
    Outline _ -> doOutline cs
    Stats _ -> doStats cs
    Lint level _ -> doLint level cs
    Test _ -> doTest cs
    TagTrees dryRun dir depth tagForUntagged _ ->
      makeTagTrees dryRun dir depth (PlainTag . T.pack <$> tagForUntagged) cs

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

doParse :: Collection -> IO ()
doParse cs = do
  putStrLn $
    "There are a total of "
      ++ show (length (cs ^.. allOrgFiles . allEntries))
      ++ " entries"
  pPrint $ countEntries cs $ \e m k ->
    k m $ case e ^. entryKeyword of
      Nothing -> "<plain>"
      Just (OpenKeyword _ kw) -> kw
      Just (ClosedKeyword _ kw) -> kw
  pPrint $ countEntries cs $ \e m k -> foldr (flip k) m (e ^. entryTags)
  forM_ (cs ^. items) $ \case
    OrgItem o ->
      putStrLn $
        o ^. orgFilePath
          ++ ": "
          ++ show (length (o ^.. entries []))
          ++ " entries"
    _ -> pure ()

doTagsList :: Collection -> IO ()
doTagsList cs = do
  let counts = countEntries cs $ \e m k -> foldr (flip k) m (e ^. entryTags)
  forM_ (M.toList counts) $ \(tag, cnt) ->
    putStrLn $ show cnt ++ " " ++ T.unpack (tag ^. tagText)

doCategoriesList :: Collection -> IO ()
doCategoriesList cs = do
  let counts = countEntries cs $ \e m k -> k m (e ^. entryCategory)
  forM_ (M.toList counts) $ \(cat, cnt) ->
    putStrLn $ show cnt ++ " " ++ T.unpack cat

doPrint :: Collection -> IO ()
doPrint cs =
  forM_ (cs ^. items) $ \case
    OrgItem o -> T.putStrLn $ _OrgFile globalConfig (o ^. orgFilePath) # o
    _ -> pure ()

doDump :: Collection -> IO ()
doDump = pPrint

doOutline :: Collection -> IO ()
doOutline cs =
  mapM_
    (mapM_ T.putStrLn . concatMap summarizeEntry)
    (cs ^.. allOrgFiles . orgFileEntries)

doStats :: Collection -> IO ()
doStats cs =
  mapM_
    (mapM_ T.putStrLn . concatMap summarizeEntry)
    (cs ^.. allOrgFiles . orgFileEntries)

doLint :: LintMessageKind -> Collection -> IO ()
doLint level cs = do
  putStrLn $
    "Linting "
      ++ show (length (cs ^.. allOrgFiles . allEntries))
      ++ " entries ("
      ++ show
        ( length
            ( filter
                (\e -> maybe False isTodo (e ^? keyword))
                (cs ^.. allOrgFiles . allEntries)
            )
        )
      ++ " todo entries) across "
      ++ show (length (cs ^.. allOrgFiles))
      ++ " org files"
  case lintCollection globalConfig level cs of
    [] -> do
      putStrLn "Pass."
      exitSuccess
    xs -> do
      mapM_ (putStrLn . showLintOrg) xs
      exitWith (ExitFailure (length xs))

doTest :: Collection -> IO ()
doTest cs = do
  forM_ (cs ^. pre (items . traverse)) $ \f -> do
    putStrLn "filePath:"
    pPrint $ f ^. filePath
    putStrLn "fileTitle:"
    pPrint $ f ^. fileTitle
    putStrLn "fileTimestamp:"
    pPrint $ f ^? fileTimestamp . to showTime
    putStrLn "fileCreatedTime:"
    pPrint $ f ^? fileCreatedTime . to showTime
  forM_ (cs ^. pre (allOrgFiles . allEntries)) $ \e -> do
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
