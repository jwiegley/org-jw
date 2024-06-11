{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad.Except
import Data.ByteString qualified as B
import Data.Foldable (forM_)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
  paths <- case opts ^. inputs of
    FileFromStdin -> pure ["<stdin>"]
    Paths paths -> pure paths
    ListFromStdin -> map T.unpack . T.lines . T.decodeUtf8 <$> readStdin
    FilesFromFile path -> readLines path
  cs <-
    runExceptT (readCollection globalConfig paths) >>= \case
      Left msg -> do
        putStrLn $ "Cannot parse: " ++ msg
        exitWith (ExitFailure 1)
      Right x -> pure x
  doLint (opts ^. kind) cs

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
  let m = lintCollection globalConfig level cs
  if M.null m
    then do
      putStrLn "Pass."
      exitSuccess
    else do
      _ <- flip M.traverseWithKey m $ \path msgs -> do
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
        forM_ msgs' $ \msg ->
          putStrLn $ showLintOrg path msg
      exitWith (ExitFailure (M.size m))
