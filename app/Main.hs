module Main where

import qualified Data.Map as Map

import Language.Lex
import Language.Par
import Language.Abs
import Language.ErrM

data Value = Integer Integer | Bool Bool deriving (Show)

add (Integer v1) (Integer v2) = Integer (v1 + v2)
sub (Integer v1) (Integer v2) = Integer (v1 - v2)
multiply (Integer v1) (Integer v2) = Integer (v1 * v2)
divide (Integer v1) (Integer v2) = Integer (v1 `div` v2)

data Env = Env (Map.Map Ident Value) deriving (Show)
startEnv = Env (Map.empty)
withVariable ident val (Env vars) = Env (Map.insert ident val vars)
getVariable ident (Env vars) = vars Map.! ident

interpretExpr :: Expr -> Env -> Value
interpretExpr x env = case x of
  EAdd expr0 expr1  -> add (interpretExpr expr0 env) (interpretExpr expr1 env)
  ESub expr0 expr1  -> sub (interpretExpr expr0 env) (interpretExpr expr1 env)
  EMul expr0 expr1  -> multiply (interpretExpr expr0 env) (interpretExpr expr1 env)
  EDiv expr0 expr1  -> divide (interpretExpr expr0 env) (interpretExpr expr1 env)
  EInt n -> Integer n
  ETrue -> Bool True 
  EFalse -> Bool False
  EVar ident -> getVariable ident env
  ESemicolon stmt expr -> (interpretExpr expr) . (interpretStmt stmt) $ env

interpretStmt :: Stmt -> Env -> Env
interpretStmt stmt env = case stmt of
  SDeclVar ident expr -> withVariable ident (interpretExpr expr env) env

main = do
    interact calc
    putStrLn ""

calc s =
    let Ok e = pExpr (myLexer s)
    in show (interpretExpr e startEnv)