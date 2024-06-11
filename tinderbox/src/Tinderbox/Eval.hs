{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tinderbox.Eval where

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

evalExpr :: (MonadError String m) => Expr -> Entry -> m Text
evalExpr x e = case x of
  Sym _ -> error "Sym not implemented"
  Var nm ->
    case e ^? anyProperty nm of
      Nothing -> throwError $ T.unpack $ "Unknown property " <> nm
      Just v -> pure v
  Path _ -> error "Path not implemented"
  Dot _sub _nm -> error "Dot not implemented"
  Access _ _ -> error "Access not implemented"
