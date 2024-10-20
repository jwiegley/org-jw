{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Org.Site.Exec where

import Control.Lens
import Data.Time
import Hakyll
import Org.Site
import Org.Site.Options
import Org.Types
import Prelude hiding (readFile)

execSite :: Config -> SiteOptions -> Collection -> IO ()
execSite _cfg opts (Collection _xs) = do
  now <- getCurrentTime
  siteConfig <- readSiteConfiguration (opts ^. configFile)
  hakyllWith
    defaultConfiguration
      { provideMetadata = pandocMetadata (Just (siteName siteConfig)),
        inMemoryCache = True,
        deployCommand = siteDeploy siteConfig
      }
    (siteRules now siteConfig)
