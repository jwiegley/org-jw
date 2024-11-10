{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Site.Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Hakyll qualified as H
import Hakyll.Core.Runtime qualified
import Options.Applicative as OA

deriving instance Eq Hakyll.Core.Runtime.RunMode

deriving instance Eq H.Command

data SiteOptions = SiteOptions
  { _hakyllCommand :: H.Command
  }
  deriving (Show, Eq, Typeable, Generic)

makeLenses ''SiteOptions

siteOptions :: Parser SiteOptions
siteOptions = SiteOptions <$> H.commandParser H.defaultConfiguration
