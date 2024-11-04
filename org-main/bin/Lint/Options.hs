{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lint.Options where

import Control.Lens hiding (argument)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA
import Org.Lint

data LintOptions = LintOptions
  { _kind :: !LintMessageKind
  }
  deriving (Show, Typeable, Generic)

makeLenses ''LintOptions

lintOptions :: Parser LintOptions
lintOptions =
  LintOptions
    <$> option
      (maybeReader parseLintMessageKind)
      ( short 'l'
          <> long "level"
          <> value LintInfo
          <> help "Log level to report"
      )
