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
  let Config {..} = cfg
  forM_ (coll ^.. items . traverse . _OrgItem) $ \org -> do
    withSystemTempFile "roundtrip" $ \tmp h -> do
      forM_ (showOrgFile _propertyColumn _tagsColumn org) $
        hPutStrLn h
      if _changeInPlace opts
        then withFile (org ^. orgFilePath) WriteMode $ \orgh ->
          forM_ (showOrgFile _propertyColumn _tagsColumn org) $ \line ->
            hPutStr orgh line
        else
          void $
            system $
              "diff -U3 \""
                <> tmp
                <> "\" \""
                <> org ^. orgFilePath
                <> "\""
