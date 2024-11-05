{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trip.Exec where

import Control.Lens hiding ((<.>))
import Control.Monad (void)
import Data.Foldable (forM_)
import Org.Data
import Org.Print
import Org.Types
import System.IO hiding (readFile)
import System.IO.Temp
import System.Process
import Trip.Options
import Prelude hiding (readFile)

execTrip :: Config -> TripOptions -> Collection -> IO ()
execTrip cfg opts coll = do
  forM_ (coll ^.. items . traverse . _OrgItem) $ \org ->
    if _changeInPlace opts
      then withFile (org ^. orgFilePath) WriteMode $ \h ->
        writeOrgFile h org
      else withSystemTempFile "roundtrip" $ \tmp h -> do
        writeOrgFile h org
        void $
          system $
            "diff -U3 \""
              <> org ^. orgFilePath
              <> "\" \""
              <> tmp
              <> "\""
  where
    writeOrgFile h org = do
      forM_ (showOrgFile cfg org) $
        hPutStrLn h
      hClose h
