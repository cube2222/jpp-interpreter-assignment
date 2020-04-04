module Main where

import qualified Data.Map as Map
import Data.Either
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Language.Lex
import Language.Par
import Language.Abs
import Language.ErrM
import Language.Print

type TCM = ReaderT Env (ExceptT String Identity)

data Type = TInteger | TBool | TList Type | TNil | TFun Type Type deriving (Eq, Show)

assertType :: Expr -> [Type] -> TCM Type
assertType expr expectedTypes = (getExprType expr) >>= 
    (\actualType -> if not (elem actualType expectedTypes) 
                    then throwError $ showString "expression " . shows (printTree expr) . showString " expected to be one of types: '" . shows expectedTypes . showString "' but is of type: '" . shows actualType $ "'" 
                    else pure actualType)

--unify :: [Type] -> TCM Type
--unify expr expectedTypes = (getExprType expr) >>= 
--    (\actualType -> if not (elem actualType expectedTypes) 
--                    then throwError $ showString "expression " . shows (printTree expr) . showString " expected to be one of types: '" . shows expectedTypes . showString "' but is of type: '" . shows actualType $ "'" 
--                    else pure actualType)

getExprType :: Expr -> TCM Type
getExprType x = case x of
  EAdd expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ESub expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EMul expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EDiv expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  -- TODO: Add recursive list checking
  EEq expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ENotEq expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ELt expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGt expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ELtEq expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGtEq expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EInt n -> pure TInteger
  ETrue -> pure TBool
  EFalse -> pure TBool
  EVar ident -> undefined
  ECons x xs -> (getExprType xs) >>=
    (\tailType -> case tailType of
      TList t -> (getExprType x) >>= (\headType -> if headType == t 
                                                   then pure . TList $ headType 
                                                   else throwError $ showString "tail of cons has different type than head, have: " . shows headType . showString " and " . shows t $ "")
      TNil -> TList <$> getExprType x
      _ -> throwError $ showString "tail of cons has to be of List type, is: " . shows tailType $ ""
    )
  ENil -> pure TNil
  EList exprs -> undefined
  EIfte predicateExpr thenExpr elseExpr -> undefined
  ESemicolon stmt expr -> undefined
  EFunCall funExpr argExprs -> undefined
  ELambda argName expr -> undefined
  EMatch expr clauses -> undefined

-- Runtime

data Value = Integer Integer | Bool Bool | List [Value] | Fun Expr Env deriving (Show)

invalidArgumentsError opName v1 v2 = showString "invalid arguments to '" . showString opName . showString "': " . shows v1 . showString " and " . shows v2 $ ""

add :: Value -> Value -> IM Value
add (Integer v1) (Integer v2) = pure . Integer $ (v1 + v2)
add v1 v2 = throwError $ invalidArgumentsError "add" v1 v2
sub :: Value -> Value -> IM Value
sub (Integer v1) (Integer v2) = pure . Integer $ (v1 - v2)
sub v1 v2 = throwError $ invalidArgumentsError "sub" v1 v2
multiply :: Value -> Value -> IM Value
multiply (Integer v1) (Integer v2) = pure . Integer $ (v1 * v2)
multiply v1 v2 = throwError $ invalidArgumentsError "multiply" v1 v2
divide :: Value -> Value -> IM Value
divide (Integer v1) (Integer 0) = throwError "division by zero"
divide (Integer v1) (Integer v2) = pure . Integer $ (v1 `div` v2)
divide v1 v2 = throwError $ invalidArgumentsError "divide" v1 v2

eq :: Value -> Value -> IM Value
eq (Integer v1) (Integer v2) = pure . Bool $ (v1 == v2)
eq (Bool v1) (Bool v2) = pure . Bool $ (v1 == v2)
eq (List (v1:v1s)) (List (v2:v2s)) = eq v1 v2 >>= (\(Bool ok) -> if ok then eq (List v1s) (List v2s) else pure . Bool $ False)
eq (List []) (List []) = pure . Bool $ True
eq (List _) (List _) = pure . Bool $ False
eq v1 v2 = throwError $ invalidArgumentsError "eq" v1 v2
notEq v1 v2 = Bool . (\(Bool b) -> not b) <$> (eq v1 v2)

lt :: Value -> Value -> IM Value
lt (Integer v1) (Integer v2) = pure . Bool $ (v1 < v2)
lt v1 v2 = throwError $ invalidArgumentsError "less than" v1 v2
gt :: Value -> Value -> IM Value
gt (Integer v1) (Integer v2) = pure . Bool $ (v1 > v2)
gt v1 v2 = throwError $ invalidArgumentsError "greater than" v1 v2
ltEq :: Value -> Value -> IM Value
ltEq (Integer v1) (Integer v2) = pure . Bool $ (v1 <= v2)
ltEq v1 v2 = throwError $ invalidArgumentsError "less than or equal" v1 v2
gtEq :: Value -> Value -> IM Value
gtEq (Integer v1) (Integer v2) = pure . Bool $ (v1 >= v2)
gtEq v1 v2 = throwError $ invalidArgumentsError "greater than or equal" v1 v2

data Env = Env { variables :: Map.Map Ident Value } deriving (Show)
type IM = ReaderT Env (ExceptT String Identity)

startEnv = Env { variables = Map.empty }
withVariable :: Ident -> Value -> Env -> Env
withVariable ident val env = env { variables = Map.insert ident val (variables env) }

getVariable :: Ident -> IM Value
getVariable ident@(Ident str) = (Map.lookup) <$> (pure ident) <*> (asks variables) >>= 
  (\result -> case result of 
    Just value -> pure value
    Nothing -> throwError $ showString "unknown variable used: " . showString str $ ""
    )

getFunction :: Value -> (Ident, Expr, Env)
getFunction (Fun (ELambda argName bodyExpr) env) = (argName, bodyExpr, env)

data CaptureExpression = CaptureInteger Integer | CaptureBool Bool | CaptureCons CaptureExpression CaptureExpression  | CaptureList [CaptureExpression] | CaptureVariable Ident

interpretMatchExpression :: Expr -> IM CaptureExpression
interpretMatchExpression x = case x of
  EInt n -> pure . CaptureInteger $ n
  ETrue -> pure . CaptureBool $ True
  EFalse -> pure . CaptureBool $ False
  EVar ident -> pure . CaptureVariable $ ident
  ECons head tail -> (liftM2 CaptureCons) (interpretMatchExpression head) (interpretMatchExpression tail)
  ENil -> pure . CaptureList $ []
  EList exprs -> CaptureList <$> (foldr (\expr -> (\list -> list >>= (\list -> (\expr -> expr : list) <$> (interpretMatchExpression expr)))) (pure []) exprs)
  _ -> throwError "invalid match expression"

-- TODO This has to return ExceptT on duplicate identificator
match :: CaptureExpression -> Value -> Maybe [(Ident, Value)]
match pattern value = case (pattern, value) of
  (CaptureVariable ident, value) -> Just [(ident, value)]
  (CaptureInteger x, Integer y) -> if x == y then Just [] else Nothing
  (CaptureBool x, Bool y) -> if x == y then Just [] else Nothing
  (CaptureCons x xs, List (y:ys)) -> (match x y) >>= (\list -> (\rest -> concat [list, rest]) <$> (match xs (List ys)))
  (CaptureList (x:xs), List (y:ys)) -> (match x y) >>= (\list -> (\rest -> concat [list, rest] ) <$> (match (CaptureList xs) (List ys)))
  (CaptureList [], List []) -> Just []
  _ -> Nothing

binaryOp op expr0 expr1 = (interpretExpr expr0) >>= (\expr0 -> (interpretExpr expr1) >>= (\expr1 -> op expr0 expr1))

interpretExpr :: Expr -> IM Value
interpretExpr x = case x of
  EAdd expr0 expr1  -> binaryOp add expr0 expr1
  ESub expr0 expr1  -> binaryOp sub expr0 expr1
  EMul expr0 expr1  -> binaryOp multiply expr0 expr1
  EDiv expr0 expr1  -> binaryOp divide expr0 expr1
  EEq expr0 expr1 -> binaryOp eq expr0 expr1
  ENotEq expr0 expr1 -> binaryOp notEq expr0 expr1
  ELt expr0 expr1 -> binaryOp lt expr0 expr1
  EGt expr0 expr1 -> binaryOp gt expr0 expr1
  ELtEq expr0 expr1 -> binaryOp ltEq expr0 expr1
  EGtEq expr0 expr1 -> binaryOp gtEq expr0 expr1
  EInt n -> pure . Integer $ n
  ETrue -> pure . Bool $ True
  EFalse -> pure . Bool $ False
  EVar ident -> getVariable ident
  ECons x xs -> (interpretExpr xs) >>= (\(List xs) -> (\x -> List (x:xs)) <$> (interpretExpr x))
  ENil -> pure . List $ []
  EList exprs -> List <$> (foldr (\expr -> (\rList -> rList >>= (\list -> (\val -> val : list) <$> (interpretExpr expr)))) (pure []) exprs)
  EIfte predicateExpr thenExpr elseExpr ->
    let
      predicate = interpretExpr predicateExpr
      branch = predicate >>= (\value -> case value of
        Bool b -> pure $ if b then thenExpr else elseExpr
        v -> throwError $ showString "invalid predicate in if expression: " . shows v $ ""
        )
    in
      branch >>= interpretExpr
  ESemicolon stmt expr -> (interpretStmt stmt) >>= ((flip local) (interpretExpr expr))
  EFunCall funExpr argExprs ->
    let 
      handleSingleLevel argExpr (Fun (ELambda argName bodyExpr) env) = 
        let 
          funcParams = do
            argVal <- (interpretExpr argExpr)
            return (argName, argVal, bodyExpr, env)
          callFunc (argName, argVal, bodyExpr, env) = local (\ _ -> withVariable argName argVal env) (interpretExpr bodyExpr)
        in funcParams >>= callFunc
    in foldl (\f -> (\expr -> (f >>= handleSingleLevel expr))) (interpretExpr funExpr) argExprs
  ELambda argName expr -> Fun (ELambda argName expr) <$> ask
  EMatch expr clauses ->
    let 
      combine :: Value -> MatchClause -> Maybe ([(Ident, Value)], Expr) -> IM (Maybe ([(Ident, Value)], Expr))
      combine value (MMatchClause capture body) acc = case acc of
        Just g -> pure . Just $ g
        Nothing -> (liftM (\captured -> (captured, body))) <$> ((\captureExpr -> match captureExpr value) <$> (interpretMatchExpression capture))
      handleClauses :: [MatchClause] -> Value -> IM (Maybe ([(Ident, Value)], Expr))
      handleClauses clauses value =
        foldl (\acc -> (\clause -> acc >>= (\acc -> combine value clause acc))) (pure Nothing) clauses
      capture = (interpretExpr expr) >>= (handleClauses clauses) 
      handleMatch match = case match of
        Just (captured, body) -> local (\env -> foldl (\env -> (\(name, value) -> withVariable name value env)) env captured) (interpretExpr body)
        Nothing -> throwError "Unexhaustive match."
    in capture >>= handleMatch

interpretStmt :: Stmt -> IM (Env -> Env)
interpretStmt stmt = case stmt of
  SDeclVar ident expr -> (withVariable ident) <$> (interpretExpr expr)
  SDeclFun fName argNames expr -> 
    let
      makeFun env = 
        let
          recEnv = withVariable fName recFun env
          recFun = Fun (foldr (\argName -> (\expr -> (ELambda argName expr))) expr argNames) recEnv
        in recFun
    in (withVariable fName) <$> (makeFun <$> ask)

main = do
    interact calc
    putStrLn ""

calc s =
    let Ok e = pExpr (myLexer s)
        outType = runIdentity (runExceptT (runReaderT (getExprType e) startEnv))
    in case outType of
        Left e -> "Type Error: " ++ e
        Right typeName -> show typeName
          --let 
          --  out = runIdentity (runExceptT (runReaderT (interpretExpr e) startEnv))
          --in case out of
          --  Left e -> "Runtime Error: " ++ e
          --  Right val -> show val
