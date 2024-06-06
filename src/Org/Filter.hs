{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Org.Filter where

import Control.Lens
import Data.Data
import GHC.Generics
import Org.Types

data TagExpr
  = TagVar Tag
  | TagAnd TagExpr TagExpr
  | TagOr TagExpr TagExpr
  | TagNot TagExpr
  deriving (Show, Eq, Ord, Generic, Data, Typeable, Plated)
