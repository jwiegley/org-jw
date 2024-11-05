{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stats.Exec where

import Control.Lens hiding ((<.>))
import Control.Monad (void)
import Data.Foldable (forM_)
import Org.Data
import Org.Print
import Org.Types
import Stats.Options
import System.IO hiding (readFile)
import System.IO.Temp
import System.Process
import Prelude hiding (readFile)

execStats :: Config -> StatsOptions -> Collection -> IO ()
execStats cfg opts coll = do
  error "stats is not yet implemented"
