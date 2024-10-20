{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Site.Exec where

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
import Org.Site
import Org.Site.Options
import Org.Types
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

execSite :: Config -> SiteOptions -> Collection -> IO ()
execSite cfg opts (Collection xs) = undefined
