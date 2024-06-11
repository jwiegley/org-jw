{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tinderbox.Parser where

import Control.Arrow (left)
import Control.Lens
import Control.Monad.Except
import Data.Data
import Data.Hashable
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import GHC.Generics
import Org.Data
import Org.Parser
import Org.Types
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

parseOrgExpr :: BasicParser Expr
parseOrgExpr = Var <$> (char '$' *> identifier)
