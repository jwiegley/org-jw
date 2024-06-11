{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Tinderbox.Types where

import Control.Lens
import Data.Data
import Data.Hashable
import Data.List.NonEmpty
import Data.Text (Text)
import GHC.Generics

data Type
  = TBoolean
  | TColor
  | TDate
  | TDictionary
  | TInterval
  | TNumber
  | TList
  | TSet
  | TString
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

data Stmt
  = SVarDecl (Maybe Type) Text (Maybe Expr)
  | SAssign Expr Expr
  | SIf Expr [Stmt] (Maybe [Stmt])
  | SEach Expr Text [Stmt]
  | SWhile Expr [Stmt]
  | SScope [Stmt]
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

data Expr
  = ESym Text
  | EPath (NonEmpty Text)
  | EVar Text
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EDes Expr Expr -- \$Foo(bar)
  | EIf Expr Expr (Maybe Expr)
  | ELookupTable [(Text, Expr)]
  | EList [Expr]
  | EParen Expr
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

data BinOp
  = OpDot -- '.'
  | OpMul -- '*'
  | OpDiv -- '/'
  | OpAdd -- '+'
  | OpSub -- '-'
  | OpGt -- '>'
  | OpGte -- '>='
  | OpLt -- '<'
  | OpLte -- '<='
  | OpEq -- '=='
  | OpNEq -- '!='
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)

data UnOp
  = OpNot -- '!'
  | OpNeg -- '-'
  deriving (Show, Eq, Generic, Data, Typeable, Hashable, Plated)
