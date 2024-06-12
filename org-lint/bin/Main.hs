{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Data.ByteString qualified as B
import Data.Foldable (forM_)
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import FlatParse.Stateful qualified as FP
import Options
import Org.Data
import Org.Lint
import Org.Read
import Org.Types
import System.Exit
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- getOptions
  (entriesById, n) <-
    runExceptT
      ( foldCollection
          globalConfig
          (opts ^. inputs)
          (M.empty, 0)
          (doLint globalConfig (opts ^. kind))
      )
      >>= \case
        Left msg -> do
          putStrLn $ "Error: " ++ msg
          exitWith (ExitFailure 1)
        Right x -> pure x
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
      putStrLn "Pass."
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

doLint ::
  Config ->
  LintMessageKind ->
  FilePath ->
  Either String CollectionItem ->
  (Map String (NonEmpty Loc), Integer) ->
  ExceptT String IO (Map String (NonEmpty Loc), Integer)
doLint _ _level path (Left err) (entriesById, n) = liftIO $ do
  putStrLn $ path ++ ": " ++ err
  return (entriesById, succ n)
doLint _ _level _path (Right (DataItem _item)) (entriesById, n) =
  pure (entriesById, n)
doLint cfg level path (Right (OrgItem org)) (entriesById, n) = liftIO $ do
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
  contents <- B.readFile path
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
  forM_ msgs' $ putStrLn . showLintOrg path
  pure (entriesById', n + genericLength msgs)
