{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Org.Parse.Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

data ParseOptions = ParseOptions
  { _jsonDir :: !(Maybe FilePath)
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''ParseOptions

parseOptions :: OA.Parser ParseOptions
parseOptions =
  ParseOptions
    <$> optional
      ( strOption
          ( long "to-json"
              <> help "Output Org-mode files as JSON to DIR"
          )
      )
