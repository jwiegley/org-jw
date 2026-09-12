{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lint.Exec where

import Control.Lens hiding ((<.>))
import Data.ByteString qualified as B
import Data.Foldable (forM_)
import Data.List (find)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Traversable (forM)
import FlatParse.Stateful qualified as FP
import Lint.Options
import Org.Data
import Org.Lint
import Org.Print
import Org.Types
import Read
import System.Exit
import System.FilePath
import System.IO hiding (readFile)
import System.IO.Temp
import System.Process
import Prelude hiding (readFile)

execLint :: Config -> LintOptions -> Collection -> Collection -> IO ()
execLint cfg opts (Collection xs) (Collection checkXs) = do
  -- Blog-strict checks activate automatically on the :posts: filetag, so the
  -- mode no longer carries a flag; lintOrgFiles fills in _lintPostIds itself.
  -- (The deprecated --blog switch is accepted but ignored, see Lint.Options.)
  let mode = defaultLintMode
      -- The post-ID universe for [[id:...]] resolution must come from the
      -- whole corpus, but only the subset that changed since the last lint
      -- run (checkXs) is checked, round-tripped, and chk-stamped here.
      msgs =
        M.filterWithKey (\path _ -> path `Set.member` checkPaths) $
          lintOrgFiles cfg mode (opts ^. kind) allItems
      n = M.foldl' (\acc ms -> acc + length ms) 0 msgs
  ecs <- forM (M.assocs msgs) $ \(path, ms) -> case ms of
    [] -> do
      ec <-
        if opts ^. roundTrip
          then withSystemTempFile "lint-roundtrip" $ \tmp h -> do
            let Just org = find (\o -> o ^. orgFilePath == path) orgItems
            writeOrgFile h org
            system $
              "diff -U3 \""
                <> path
                <> "\" \""
                <> tmp
                <> "\""
          else pure ExitSuccess
      case ec of
        ExitSuccess ->
          forM_ (opts ^. checkDir) $ \cdir ->
            createCheckFile cdir path
        ExitFailure _ ->
          putStrLn $
            showLintOrg path (LintMessage 0 LintError FileFailsToRoundTrip)
      pure ec
    _ -> do
      ms' <- findPositions path ms
      forM_ ms' $ \msg ->
        putStrLn $ showLintOrg path msg
      pure ExitSuccess
  let n' = n + sum (map (\ec -> case ec of ExitSuccess -> 0; _ -> 1) ecs)
  if n' == 0
    then do
      putStrLn $ show (length checkXs) ++ " files passed lint"
      exitSuccess
    else exitWith (ExitFailure n')
 where
  allItems = xs ^.. traverse . _OrgItem
  checkPaths = Set.fromList (map (^. orgFilePath) (checkXs ^.. traverse . _OrgItem))
  orgItems = checkXs ^.. traverse . _OrgItem
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

  writeOrgFile h org = do
    forM_ (showOrgFile cfg org) $
      hPutStrLn h
    hClose h
