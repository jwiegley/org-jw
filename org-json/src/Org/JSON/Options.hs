{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Org.JSON.Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

data JsonOptions = JsonOptions
  { _jsonDir :: !(Maybe FilePath)
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''JsonOptions

jsonOptions :: OA.Parser JsonOptions
jsonOptions =
  JsonOptions
    <$> optional
      ( strOption
          ( short 'o'
              <> long "output"
              <> help "Output Org-mode files as JSON to DIR"
          )
      )
