{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSON.Exec where

import Control.Lens hiding ((<.>))
import Data.ByteString.Lazy qualified as B
import Data.Foldable (forM_)
import JSON.Options
import Org.Data
import Org.JSON
import Org.Types
import System.FilePath
import Prelude hiding (readFile)

execJson :: Config -> JsonOptions -> Collection -> IO ()
execJson _cfg opts coll =
  forM_ (coll ^.. items . traverse . _OrgItem) $ \org ->
    case opts ^. jsonDir of
      Just dir ->
        orgFileToJSONFile (jsonFilePath dir (org ^. orgFilePath)) org
      Nothing ->
        B.putStr $ orgFileToJSON org

jsonFilePath :: FilePath -> FilePath -> FilePath
jsonFilePath jdir path =
  jdir </> takeBaseName (map repl path) <.> "json"
  where
    repl '/' = '!'
    repl c = c
