{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.Exec where

import Control.Lens
import Data.Time
import Hakyll
import Org.Site
import Org.Types
import Site.Options
import Prelude hiding (readFile)

execSite :: Bool -> Config -> SiteOptions -> Collection -> IO ()
execSite verbose _cfg opts (Collection _xs) = do
  now <- getCurrentTime
  siteConfig <- readSiteConfiguration (opts ^. configFile)
  hakyllWithArgs
    defaultConfiguration
      { destinationDirectory = "_site",
        storeDirectory = "_cache",
        tmpDirectory = "_cache/tmp",
        providerDirectory = ".",
        deployCommand = siteDeploy siteConfig,
        inMemoryCache = True,
        previewHost = "127.0.0.1",
        previewPort = 8000,
        provideMetadata = pandocMetadata (Just (siteName siteConfig))
      }
    Options
      { verbosity = verbose,
        optCommand = opts ^. hakyllCommand
      }
    (siteRules now siteConfig)
