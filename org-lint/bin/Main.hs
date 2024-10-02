{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.ByteString qualified as B
import Data.Foldable (foldrM, forM_)
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import FlatParse.Stateful qualified as FP
import Options
import Org.Data
import Org.JSON
import Org.Lint
import Org.Read
import Org.Types
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  Collection xs <- readCollectionIO globalConfig (opts ^. inputs)
  (entriesById, n) <-
    foldrM
      ( doLint
          globalConfig
          (opts ^. jsonDir)
          (opts ^. kind)
      )
      (M.empty, 0)
      xs
  let idMsgs :: [(FilePath, [LintMessage])]
      idMsgs = flip concatMap (M.assocs entriesById) $ \(k, loc :| locs) ->
        [ ( loc ^. file,
            [ LintMessage
                (loc ^. pos)
                LintError
                (DuplicatedIdentifier k)
            ]
          )
          | not (null locs)
        ]
  forM_ idMsgs $ \(path, msgs) ->
    forM_ msgs $ \msg ->
      putStrLn $ showLintOrg path msg
  if n == 0
    then do
      putStrLn $ show (length xs) ++ " files passed lint"
      exitSuccess
    else do
      exitWith (ExitFailure (fromInteger n))

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
        "TASK"
      ]
    _closedKeywords =
      [ "DONE",
        "COMPLETE",
        "ABORTED",
        "CANCELED",
        "NOTE",
        "LINK"
      ]
    _keywordTransitions =
      [ ("TODO", ["DOING", "WAIT", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("PROJECT", ["ABORTED", "COMPLETE"]),
        ("DOING", ["TODO", "WAIT", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("WAIT", ["DOING", "TODO", "DEFER", "TASK", "CANCELED", "DONE"]),
        ("DEFER", ["DOING", "WAIT", "TODO", "TASK", "CANCELED", "DONE"]),
        ("TASK", ["DOING", "WAIT", "DEFER", "TODO", "CANCELED", "DONE"]),
        ("DONE", ["TODO", "TASK"]),
        ("ABORTED", ["PROJECT"]),
        ("COMPLETE", ["PROJECT"]),
        ("CANCELED", ["DOING", "WAIT", "DEFER", "TASK", "TODO", "DONE"])
      ]
    _priorities =
      ["A", "B", "C"]
    _propertyColumn = 11
    _tagsColumn = 97
    _attachmentsDir = "/Users/johnw/org/data"

doLint ::
  Config ->
  Maybe FilePath ->
  LintMessageKind ->
  CollectionItem ->
  (Map String (NonEmpty Loc), Integer) ->
  IO (Map String (NonEmpty Loc), Integer)
doLint _ _ _level (DataItem _item) (entriesById, n) =
  pure (entriesById, n)
doLint cfg mdir level (OrgItem org) (entriesById, n) = liftIO $ do
  forM_ mdir $ \dir ->
    orgFileToJSON (dir </> takeBaseName (org ^. orgFilePath) <.> "json") org
  let entriesById' = (\f -> foldr f entriesById (org ^.. allEntries)) $ \e m ->
        let loc = e ^. entryLoc
         in maybe
              m
              ( \ident ->
                  m
                    & at ident %~ Just . maybe (NE.singleton loc) (NE.cons loc)
              )
              (e ^? entryId)
  let msgs = execWriter $ lintOrgFile cfg level org
  contents <- B.readFile (org ^. orgFilePath)
  let poss = map (\(LintMessage p _ _) -> FP.Pos p) msgs
      linesCols = FP.posLineCols contents poss
      msgs' =
        zipWith
          ( curry
              ( \((ln, _col), LintMessage _ k c) ->
                  LintMessage (succ ln) k c
              )
          )
          linesCols
          msgs
  forM_ msgs' $ putStrLn . showLintOrg (org ^. orgFilePath)
  pure (entriesById', n + genericLength msgs)
