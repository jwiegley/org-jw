{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Org.Site.Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA
import Org.Site

data SiteOptions = SiteOptions
  { _configFile :: FilePath
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''SiteOptions

siteOptions :: Parser SiteOptions
siteOptions =
  SiteOptions
    <$> strOption
      ( short 'c'
          <> long "config"
          <> value "config.yaml"
          <> help "Config file"
      )
