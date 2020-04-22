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

data Type = TInteger | TBool | TList Type | TNil | TFun Type Type deriving (Eq, Show)

data TEnv = TEnv { variable_types :: Map.Map Ident Type } deriving (Show)
type TCM = ReaderT TEnv (ExceptT String Identity)

startTEnv = TEnv { variable_types = Map.empty }
withVariableType :: Ident -> Type -> TEnv -> TEnv
withVariableType ident t env = env { variable_types = Map.insert ident t (variable_types env) }

getVariableType :: Ident -> TCM Type
getVariableType ident@(Ident str) = (Map.lookup) <$> (pure ident) <*> (asks variable_types) >>= 
  (\result -> case result of 
    Just t -> pure t
    Nothing -> throwError $ showString "unknown variable used: " . showString str $ ""
    )

getType :: TypeName -> TCM Type -- TODO: add custom type handling
getType typeName = case typeName of
  TSimpleTypeName (Ident name) -> case name of
    "Int" -> pure TInteger
    "Bool" -> pure TBool
    _ -> (throwError $
      showString "unknown simple type name: " .
      showString name $
      "")
  TPolymorphicTypeName (Ident name) params -> case name of -- TODO: Multiargument function syntax sugar support, or maybe not?
    "List" -> do
      when ((length params) /= 1) (throwError $
        showString "exactly 1 type argument expected for list, got " .
        shows (length params) .
        showString ": " .
        shows (printTree typeName) $
        "")
      elementType <- getType (head params)
      return (TList elementType)
    

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
  EEq expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ENotEq expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ELt expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGt expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ELtEq expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGtEq expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EInt n -> pure TInteger
  ETrue -> pure TBool
  EFalse -> pure TBool
  EVar ident -> getVariableType ident
  ECons x xs -> (getExprType xs) >>=
    (\tailType -> case tailType of
      TList t -> (getExprType x) >>= (\headType -> if headType == t 
                                                   then pure . TList $ headType 
                                                   else throwError $ showString "tail of cons has different type than head, have: " . shows headType . showString " and " . shows t $ "")
      TNil -> TList <$> getExprType x
      _ -> throwError $ showString "tail of cons has to be of List type, is: " . shows tailType $ ""
    )
  ENil -> pure TNil
  EList [] -> pure TNil
  EList (x:xs) -> let 
    checkUniformType headType xs =
      foldl (\acc -> (\curExpr -> do
        accType <- acc
        curType <- getExprType curExpr
        when (accType /= curType) (throwError $ 
          showString "found expressions of differing types in list literal: " . 
          shows (printTree x) . showString " of type " . shows accType .
          showString " and " . 
          shows (printTree curExpr) . showString " of type " . shows curType $ 
          "")
        return accType
      )) headType xs
    in TList <$> checkUniformType (getExprType x) xs
  EIfte predicateExpr thenExpr elseExpr -> do
    predicateType <- getExprType predicateExpr
    when (predicateType /= TBool) (throwError $ 
      showString "predicate in if expression must be boolean, is: " . 
      shows (printTree predicateExpr) . showString " of type " . shows predicateType $
      "")
    thenType <- getExprType thenExpr
    elseType <- getExprType elseExpr
    when (thenType /= elseType) (throwError $ 
      showString "found expressions of differing types in if-then-else branches: " . 
      shows (printTree thenExpr) . showString " of type " . shows thenType .
      showString " and " . 
      shows (printTree elseExpr) . showString " of type " . shows elseType $ 
      "")
    return thenType
  ESemicolon stmt expr -> (typecheckStmt stmt) >>= ((flip local) (getExprType expr))
  EFunCall funExpr argExprs -> do
    funExprType <- getExprType funExpr
    resultType <- foldl (\t -> (\arg -> do
      funType <- t
      newFunType <- case funType of
                      TFun inputType outputType -> do
                        assertType arg [inputType]
                        return outputType
                      _ -> (throwError $
                          showString "trying to call expression of non-function type " .
                          shows funType $
                          ""
                        )
      return newFunType
      )) (pure funExprType) argExprs
    return resultType
  ELambda (AFunctionArgument argName argTypeName) expr -> do
    argType <- getType argTypeName
    exprType <- local (withVariableType argName argType) (getExprType expr)
    return (TFun argType exprType)
  EMatch expr clauses -> undefined

typecheckStmt :: Stmt -> TCM (TEnv -> TEnv)
typecheckStmt stmt = case stmt of
  SDeclVar ident expr -> (withVariableType ident) <$> (getExprType expr)
  SDeclFun fName args expr -> (\t -> withVariableType fName t) <$>
                                    do
                                      argsParsed <- foldr (\(AFunctionArgument name argType) -> (\argsParsed -> do
                                        argsParsed <- argsParsed
                                        argType <- getType argType
                                        return ((name, argType) : argsParsed)
                                        )) (pure []) args
                                      bodyType <- local (\tenv -> (foldl (\tenv -> (\(argName, argType) -> withVariableType argName argType tenv)) tenv argsParsed)) (getExprType expr)
                                      functionType <- pure (foldr (\(argName, argType) -> (\bodyType -> TFun argType bodyType)) bodyType argsParsed)
                                      return functionType
                                      
  -- 
    -- let
    --   makeFun env = 
    --     let
    --       recEnv = withVariable fName recFun env
    --       recFun = Fun (foldr (\argName -> (\expr -> (ELambda argName (TSimpleTypeName (Ident "TODOFixMe")) expr))) expr argNames) recEnv -- TODO: Fix this lambda type
    --     in recFun
    -- in (withVariable fName) <$> (makeFun <$> ask)

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
getFunction (Fun (ELambda (AFunctionArgument argName (TSimpleTypeName (Ident "TODOFixMe"))) bodyExpr) env) = (argName, bodyExpr, env) -- TODO: Fix this lambda type

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

-- TODO This has to return ExceptT on duplicate identifier
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
      handleSingleLevel argExpr (Fun (ELambda (AFunctionArgument argName (TSimpleTypeName (Ident "TODOFixMe"))) bodyExpr) env) =   -- TODO: Fix this lambda type
        let 
          funcParams = do
            argVal <- (interpretExpr argExpr)
            return (argName, argVal, bodyExpr, env)
          callFunc (argName, argVal, bodyExpr, env) = local (\ _ -> withVariable argName argVal env) (interpretExpr bodyExpr)
        in funcParams >>= callFunc
    in foldl (\f -> (\expr -> (f >>= handleSingleLevel expr))) (interpretExpr funExpr) argExprs
  ELambda (AFunctionArgument argName _) expr -> Fun (ELambda (AFunctionArgument argName (TSimpleTypeName (Ident "TODOFixMe"))) expr) <$> ask -- TODO: Fix this lambda type
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
          recFun = Fun (foldr (\(AFunctionArgument argName _) -> (\expr -> (ELambda (AFunctionArgument argName (TSimpleTypeName (Ident "TODOFixMe"))) expr))) expr argNames) recEnv -- TODO: Fix this lambda type
        in recFun
    in (withVariable fName) <$> (makeFun <$> ask)

main = do
    interact calc
    putStrLn ""

calc s =
    let parserOut = pExpr (myLexer s)
    in case parserOut of
      Bad err -> show err
      Ok expr ->
        let
          outType = runIdentity (runExceptT (runReaderT (getExprType expr) startTEnv))
        in case outType of
          Left e -> "Type Error: " ++ e
          Right typeName -> show typeName
            --let 
            --  out = runIdentity (runExceptT (runReaderT (interpretExpr e) startEnv))
            --in case out of
            --  Left e -> "Runtime Error: " ++ e
            --  Right val -> show val
