{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Trip.Options where

import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

data TripOptions = TripOptions
  { _changeInPlace :: !Bool
  }
  deriving (Show, Eq, Typeable, Generic)

tripOptions :: Parser TripOptions
tripOptions =
  TripOptions
    <$> switch
      ( long "change-in-place"
          <> help "If used, replace original file instead of generating diff"
      )
