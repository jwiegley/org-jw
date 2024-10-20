{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Lint.Exec where

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
import Org.Data
import Org.Lint
import Org.Lint.Options
import Org.Types
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

execLint :: Config -> LintOptions -> Collection -> IO ()
execLint cfg opts (Collection xs) = do
  (entriesById, n) <- foldrM (doLint (opts ^. kind)) (M.empty, 0) xs
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
  where
    doLint ::
      LintMessageKind ->
      CollectionItem ->
      (Map String (NonEmpty Loc), Integer) ->
      IO (Map String (NonEmpty Loc), Integer)
    doLint _level (DataItem _item) (entriesById, n) =
      pure (entriesById, n)
    doLint level (OrgItem org) (entriesById, n) = liftIO $ do
      let entriesById' =
            (\f -> foldr f entriesById (org ^.. allEntries)) $ \e m ->
              let loc = e ^. entryLoc
               in maybe
                    m
                    ( \ident ->
                        m
                          & at ident
                            %~ Just
                              . maybe
                                (NE.singleton loc)
                                (NE.cons loc)
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
