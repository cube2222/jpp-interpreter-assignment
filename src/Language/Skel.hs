module Language.Skel where

-- Haskell module generated by the BNF converter

import Language.Abs
import Language.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EAdd expr1 expr2 -> failure x
  ESub expr1 expr2 -> failure x
  EMul expr1 expr2 -> failure x
  EDiv expr1 expr2 -> failure x
  EInt integer -> failure x
  EOr expr1 expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  ENot expr -> failure x
  ETrue -> failure x
  EFalse -> failure x
  EVar ident -> failure x
  EFunCall ident expr -> failure x
  ESemicolon stmt expr -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  SDeclVar ident expr -> failure x
  SDeclFun ident1 ident2 expr -> failure x

