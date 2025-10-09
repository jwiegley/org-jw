{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.Exec where

import Data.Time
import Hakyll as Hakyll
import Options as Org
import Org.Site
import Org.Types
import Site.Options as Site
import System.Directory
import System.Exit
import System.FilePath
import Prelude hiding (readFile)

execSite :: Org.Options -> SiteOptions -> Collection -> IO ()
execSite opts siteOpts (Collection (DataItem config : _)) = do
  now <- getCurrentTime
  siteConfig <- readSiteConfiguration config
  withCurrentDirectory (takeDirectory config) $
    hakyllWithArgs
      defaultConfiguration
        { destinationDirectory = "_site",
          storeDirectory = "_cache",
          tmpDirectory = "_cache/tmp",
          providerDirectory = ".",
          deployCommand = siteDeploy siteConfig,
          inMemoryCache = True,
          previewHost = "127.0.0.1",
          previewPort = 8000
        }
      Hakyll.Options
        { verbosity = Org.verbose opts,
          optCommand = Site._hakyllCommand siteOpts
        }
      (siteRules now siteConfig)
execSite _ _ _ = do
  putStrLn "usage: org site <hakyll command> <config.yaml>"
  exitFailure
