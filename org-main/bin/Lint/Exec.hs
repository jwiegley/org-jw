{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lint.Exec where

import Control.Lens hiding ((<.>))
import Data.ByteString qualified as B
import Data.Foldable (forM_)
import Data.Map qualified as M
import FlatParse.Stateful qualified as FP
import Lint.Options
import Org.Data
import Org.Lint
import Org.Types
import Read
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

execLint :: Config -> LintOptions -> Collection -> IO ()
execLint cfg opts (Collection xs) = do
  let msgs = lintOrgFiles cfg (opts ^. kind) (xs ^.. traverse . _OrgItem)
      n = M.foldl' (\acc ms -> acc + length ms) 0 msgs
  forM_ (M.assocs msgs) $ \(path, ms) -> case ms of
    [] ->
      forM_ (opts ^. checkDir) $ \cdir ->
        createCheckFile cdir path
    _ -> do
      ms' <- findPositions path ms
      forM_ ms' $ \msg ->
        putStrLn $ showLintOrg path msg
  if n == 0
    then do
      putStrLn $ show (length xs) ++ " files passed lint"
      exitSuccess
    else exitWith (ExitFailure n)
  where
    findPositions :: FilePath -> [LintMessage] -> IO [LintMessage]
    findPositions path msgs = do
      contents <- B.readFile path
      let poss = map (\(LintMessage p _ _) -> FP.Pos p) msgs
          linesCols = FP.posLineCols contents poss
      pure $
        zipWith
          ( curry
              ( \((ln, _col), LintMessage _ k c) ->
                  LintMessage (succ ln) k c
              )
          )
          linesCols
          msgs
