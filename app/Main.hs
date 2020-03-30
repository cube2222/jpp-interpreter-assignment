module Main where

import qualified Data.Map as Map
import Control.Monad.Reader

import Language.Lex
import Language.Par
import Language.Abs
import Language.ErrM

data Value = Integer Integer | Bool Bool | Fun Expr Env deriving (Show)

add (Integer v1) (Integer v2) = Integer (v1 + v2)
sub (Integer v1) (Integer v2) = Integer (v1 - v2)
multiply (Integer v1) (Integer v2) = Integer (v1 * v2)
divide (Integer v1) (Integer v2) = Integer (v1 `div` v2)

eq (Integer v1) (Integer v2) = Bool (v1 == v2)
eq (Bool v1) (Bool v2) = Bool (v1 == v2)
notEq (Integer v1) (Integer v2) = Bool (v1 /= v2)
notEq (Bool v1) (Bool v2) = Bool (v1 /= v2)

lt (Integer v1) (Integer v2) = Bool (v1 < v2)
gt (Integer v1) (Integer v2) = Bool (v1 > v2)
ltEq (Integer v1) (Integer v2) = Bool (v1 <= v2)
gtEq (Integer v1) (Integer v2) = Bool (v1 >= v2)

data Env = Env { variables :: Map.Map Ident Value } deriving (Show)
type R a = Reader Env a

startEnv = Env { variables = Map.empty }
withVariable :: Ident -> Value -> Env -> Env
withVariable ident val env = env { variables = Map.insert ident val (variables env) }

getVariable :: Ident -> R Value
getVariable ident = (Map.!) <$> (asks variables) <*> pure ident

getFunction :: Value -> (Ident, Expr, Env)
getFunction (Fun (ELambda argName bodyExpr) env) = (argName, bodyExpr, env)

interpretExpr :: Expr -> R Value
interpretExpr x = case x of
  EAdd expr0 expr1  -> liftM2 add (interpretExpr expr0) (interpretExpr expr1)
  ESub expr0 expr1  -> liftM2 sub (interpretExpr expr0) (interpretExpr expr1)
  EMul expr0 expr1  -> liftM2 multiply (interpretExpr expr0) (interpretExpr expr1)
  EDiv expr0 expr1  -> liftM2 divide (interpretExpr expr0) (interpretExpr expr1)
  EEq expr0 expr1 -> liftM2 eq (interpretExpr expr0) (interpretExpr expr1)
  ENotEq expr0 expr1 -> liftM2 notEq (interpretExpr expr0) (interpretExpr expr1)
  ELt expr0 expr1 -> liftM2 lt (interpretExpr expr0) (interpretExpr expr1)
  EGt expr0 expr1 -> liftM2 gt (interpretExpr expr0) (interpretExpr expr1)
  ELtEq expr0 expr1 -> liftM2 ltEq (interpretExpr expr0) (interpretExpr expr1)
  EGtEq expr0 expr1 -> liftM2 gtEq (interpretExpr expr0) (interpretExpr expr1)
  EInt n -> pure . Integer $ n
  ETrue -> pure . Bool $ True
  EFalse -> pure . Bool $ False
  EVar ident -> getVariable ident
  EIfte predicateExpr thenExpr elseExpr ->
    let
      predicate = interpretExpr predicateExpr
      branch = (\(Bool b) -> if b then thenExpr else elseExpr) <$> predicate
    in
      branch >>= interpretExpr
  ESemicolon stmt expr -> (interpretStmt stmt) >>= ((flip local) (interpretExpr expr))
  EFunCall funExpr argExprs ->
    let 
      handleSingleLevel argExpr (Fun (ELambda argName bodyExpr) env) = 
        let 
          funcParams = do
            --(argName, bodyExpr, env) <- getFunction <$> interpretExpr funExpr
            argVal <- (interpretExpr argExpr)
            return (argName, argVal, bodyExpr, env)
          callFunc (argName, argVal, bodyExpr, env) = local (\ _ -> withVariable argName argVal env) (interpretExpr bodyExpr)
        in funcParams >>= callFunc
    in foldl (\f -> (\expr -> (f >>= handleSingleLevel expr))) (interpretExpr funExpr) argExprs
  ELambda argName expr -> Fun (ELambda argName expr) <$> ask

interpretStmt :: Stmt -> R (Env -> Env)
interpretStmt stmt = case stmt of
  SDeclVar ident expr -> (withVariable ident) <$> (interpretExpr expr)
  SDeclFun fName argNames expr -> 
    let
      makeFun env = 
        let
          recEnv = withVariable fName recFun env
          recFun = Fun (foldr (\argName -> (\expr -> (ELambda argName expr))) expr argNames) recEnv
          --recFun = Fun argName exprs recEnv
        in recFun
    in (withVariable fName) <$> (makeFun <$> ask)

main = do
    interact calc
    putStrLn ""

calc s =
    let Ok e = pExpr (myLexer s)
    in show (runReader (interpretExpr e) startEnv)