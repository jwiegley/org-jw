{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Stats.Options where

import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

data StatsOptions = StatsOptions
  { _changeInPlace :: !Bool
  }
  deriving (Show, Typeable, Generic)

statsOptions :: Parser StatsOptions
statsOptions =
  StatsOptions
    <$> switch
      ( long "change-in-place"
          <> help "If used, replace original file instead of generating diff"
      )
