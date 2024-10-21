{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Org.Site.Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Hakyll qualified as H
import Options.Applicative as OA

data SiteOptions = SiteOptions
  { _configFile :: FilePath,
    _hakyllCommand :: H.Command
  }
  deriving (Show, Typeable, Generic)

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
    <*> H.commandParser H.defaultConfiguration
