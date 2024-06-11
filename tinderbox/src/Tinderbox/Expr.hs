{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tinderbox.Expr where

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

{-

Tinderbox Expression Language

expr ::
    symbol
  | path
  | variable
  | funcall
  | access
  | binop
  | unop
  | list
  | literal_list
  | NUMBER
  | STRING

symbol ::
    IDENTIFIER

-- Paths are only accepted in certain contexts
path ::
    path-segment
  | path-segment '/' path

path-segment ::
    IDENTIFIER
  | ".."

variable ::
    '$' symbol

funcall ::
    symbol '(' funargs ')'

funargs ::
    expr
  | expr ',' funargs

binop ::
    expr BINOP expr

BINOP ::
    '*'
    '/'
    '+'
    '-'
    '>'
    '>='
    '<'
    '<='
    '='
    '=='
    '!='

unop ::
    UNOP expr

UNOP ::
    '!'
    '-'

access ::
    expr '(' expr ')'

list ::
    expr
  | expr ';' expr

literal_list ::
    '[' list ']'
-}

expr :: Text -> Either String Expr
expr str =
  left
    errorBundlePretty
    (runParser parseOrgExpr "<expr>" str)
