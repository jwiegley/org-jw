{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.JSON.Exec where

import Control.Lens hiding ((<.>))
import Data.Foldable (forM_)
import Org.Data
import Org.JSON
import Org.JSON.Options
import Org.Types
import System.FilePath
import Prelude hiding (readFile)

execJson :: Config -> JsonOptions -> Collection -> IO ()
execJson _cfg opts coll =
  forM_ (coll ^.. items . traverse . _OrgItem) $ \org -> do
    putStrLn $
      org ^. orgFilePath
        ++ ": "
        ++ show (length (org ^.. entries []))
        ++ " entries"
    forM_ (opts ^. jsonDir) $ \dir ->
      orgFileToJSON (dir </> takeBaseName (org ^. orgFilePath) <.> "json") org
