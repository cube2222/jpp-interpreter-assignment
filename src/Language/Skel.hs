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
transTypeName :: Show a => TypeName a -> Result
transTypeName x = case x of
  TSimpleTypeName _ ident -> failure x
  TPolymorphicTypeName _ ident typenames -> failure x
transFunctionParameter :: Show a => FunctionParameter a -> Result
transFunctionParameter x = case x of
  AFunctionArgument _ ident typename -> failure x
transStmt :: Show a => Stmt a -> Result
transStmt x = case x of
  SDeclVar _ ident expr -> failure x
  SDeclFun _ ident functionparameters typename expr -> failure x
transMatchClause :: Show a => MatchClause a -> Result
transMatchClause x = case x of
  MMatchClause _ expr1 expr2 -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
  EAdd _ expr1 expr2 -> failure x
  ESub _ expr1 expr2 -> failure x
  EMul _ expr1 expr2 -> failure x
  EDiv _ expr1 expr2 -> failure x
  EEq _ expr1 expr2 -> failure x
  ENotEq _ expr1 expr2 -> failure x
  ELt _ expr1 expr2 -> failure x
  EGt _ expr1 expr2 -> failure x
  ELtEq _ expr1 expr2 -> failure x
  EGtEq _ expr1 expr2 -> failure x
  EInt _ integer -> failure x
  ENegInt _ integer -> failure x
  EOr _ expr1 expr2 -> failure x
  EAnd _ expr1 expr2 -> failure x
  ENot _ expr -> failure x
  ETrue _ -> failure x
  EFalse _ -> failure x
  ELambda _ functionparameter expr -> failure x
  EVar _ ident -> failure x
  EFunCall _ expr exprs -> failure x
  ECons _ expr1 expr2 -> failure x
  EList _ exprs -> failure x
  ENil _ -> failure x
  EIfte _ expr1 expr2 expr3 -> failure x
  ESemicolon _ stmt expr -> failure x
  EMatch _ expr matchclauses -> failure x

