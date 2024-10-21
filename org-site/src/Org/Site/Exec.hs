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

execSite :: Bool -> Config -> SiteOptions -> Collection -> IO ()
execSite verbose _cfg opts (Collection _xs) = do
  now <- getCurrentTime
  siteConfig <- readSiteConfiguration (opts ^. configFile)
  hakyllWithArgs
    defaultConfiguration
      { provideMetadata = pandocMetadata (Just (siteName siteConfig)),
        inMemoryCache = True,
        deployCommand = siteDeploy siteConfig
      }
    Options
      { verbosity = verbose,
        optCommand = opts ^. hakyllCommand
      }
    (siteRules now siteConfig)
