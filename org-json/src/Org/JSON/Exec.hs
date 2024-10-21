{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.JSON.Exec where

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
import Org.JSON
import Org.JSON.Options
import Org.Types
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

execJson :: Config -> JsonOptions -> Collection -> IO ()
execJson cfg opts coll =
  forM_ (coll ^.. items . traverse . _OrgItem) $ \org -> do
    putStrLn $
      org ^. orgFilePath
        ++ ": "
        ++ show (length (org ^.. entries []))
        ++ " entries"
    forM_ (opts ^. jsonDir) $ \dir ->
      orgFileToJSON (dir </> takeBaseName (org ^. orgFilePath) <.> "json") org
