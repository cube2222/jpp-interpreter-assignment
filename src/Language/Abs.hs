-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Language.Abs where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Expr
    = EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EEq Expr Expr
    | ENotEq Expr Expr
    | ELt Expr Expr
    | EGt Expr Expr
    | ELtEq Expr Expr
    | EGtEq Expr Expr
    | EInt Integer
    | EOr Expr Expr
    | EAnd Expr Expr
    | ENot Expr
    | ETrue
    | EFalse
    | ELambda Ident Expr
    | EVar Ident
    | EFunCall Expr [Expr]
    | EList [Expr]
    | ECons Expr Expr
    | ENil
    | EIfte Expr Expr Expr
    | ESemicolon Stmt Expr
    | EMatch Expr [MatchClause]
  deriving (Eq, Ord, Show, Read)

data Stmt = SDeclVar Ident Expr | SDeclFun Ident [Ident] Expr
  deriving (Eq, Ord, Show, Read)

data MatchClause = MMatchClause Expr Expr
  deriving (Eq, Ord, Show, Read)

