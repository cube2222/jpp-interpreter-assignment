module Main where

import qualified Data.Map as Map
import Control.Monad.Reader

import Language.Lex
import Language.Par
import Language.Abs
import Language.ErrM

data Value = Integer Integer | Bool Bool | Fun Ident Expr deriving (Show)

add (Integer v1) (Integer v2) = Integer (v1 + v2)
sub (Integer v1) (Integer v2) = Integer (v1 - v2)
multiply (Integer v1) (Integer v2) = Integer (v1 * v2)
divide (Integer v1) (Integer v2) = Integer (v1 `div` v2)

data Env = Env { variables :: Map.Map Ident Value } deriving (Show)
type R a = Reader Env a

startEnv = Env { variables = Map.empty }
withVariable :: Ident -> Value -> Env -> Env
withVariable ident val env = env { variables = Map.insert ident val (variables env) }

getVariable :: Ident -> R Value
getVariable ident = (Map.!) <$> (asks variables) <*> pure ident

getFunction :: Ident -> R (Ident, Expr)
getFunction fName = (\(Fun argName expr) -> (argName, expr)) <$> getVariable fName

interpretExpr :: Expr -> R Value
interpretExpr x = case x of
  EAdd expr0 expr1  -> liftM2 add (interpretExpr expr0) (interpretExpr expr1)
  ESub expr0 expr1  -> liftM2 sub (interpretExpr expr0) (interpretExpr expr1)
  EMul expr0 expr1  -> liftM2 multiply (interpretExpr expr0) (interpretExpr expr1)
  EDiv expr0 expr1  -> liftM2 divide (interpretExpr expr0) (interpretExpr expr1)
  EInt n -> pure . Integer $ n
  ETrue -> pure . Bool $ True
  EFalse -> pure . Bool $ False
  EVar ident -> getVariable ident
  ESemicolon stmt expr -> (interpretStmt stmt) >>= ((flip local) (interpretExpr expr))
  EFunCall ident argExpr -> 
    let 
      funcParams = do
        (argName, bodyExpr) <- getFunction ident
        argVal <- (interpretExpr argExpr)
        return (argName, argVal, bodyExpr)
      callFunc (argName, argVal, bodyExpr) = local (withVariable argName argVal) (interpretExpr bodyExpr)
    in funcParams >>= callFunc

    -- (local (withVariable argName argVal) (interpretExpr bodyExpr))

interpretStmt :: Stmt -> R (Env -> Env)
interpretStmt stmt = case stmt of
  SDeclVar ident expr -> (withVariable ident) <$> (interpretExpr expr)
  SDeclFun fName argName expr -> pure $ (withVariable fName) $ (Fun argName expr)

main = do
    interact calc
    putStrLn ""

calc s =
    let Ok e = pExpr (myLexer s)
    in show (runReader (interpretExpr e) startEnv)