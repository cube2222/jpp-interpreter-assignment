module Interpreter where

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

data Type = TInteger | TBool | TList Type | TNil | TFun Type Type deriving (Eq)

instance Show Type where
  show TInteger = "Int"
  show TBool = "Bool"
  show (TList t) = "List<" ++ (show t) ++ ">"
  show TNil = "[]"
  show (TFun inType outType) = "Fun<" ++ (show inType) ++ ", " ++ (show outType) ++ ">"

data TEnv = TEnv { variable_types :: Map.Map Ident Type } deriving (Show)
type TCM = ReaderT TEnv (ExceptT String Identity)

type TCExpr = Expr (Maybe (Int, Int))
type TCStmt = Stmt (Maybe (Int, Int))
type TCTypeName = TypeName (Maybe (Int, Int))

startTEnv = TEnv { variable_types = Map.empty }
withVariableType :: Ident -> Type -> TEnv -> TEnv
withVariableType ident t env = env { variable_types = Map.insert ident t (variable_types env) }

getVariableType :: Ident -> TCM Type
getVariableType ident@(Ident str) = (Map.lookup) <$> (pure ident) <*> (asks variable_types) >>= 
  (\result -> case result of 
    Just t -> pure t
    Nothing -> throwError $ showString "unknown variable used: " . showString str $ ""
    )

getType :: TCTypeName -> TCM Type -- TODO: add custom type handling
getType typeName = case typeName of
  TSimpleTypeName _ (Ident name) -> case name of
    "Int" -> pure TInteger
    "Bool" -> pure TBool
    _ -> (throwError $
      showString "unknown simple type name: " .
      showString name $
      "")
  TPolymorphicTypeName _ (Ident name) params -> case name of
    "List" -> do
      when ((length params) /= 1) (throwError $
        showString "exactly 1 type argument expected for list, got " .
        shows (length params) . 
        showString ": " .
        shows (printTree typeName) $
        "")
      elementType <- getType (head params)
      return (TList elementType)
    "Fun" -> do
      when ((length params) /= 2) (throwError $
        showString "exactly 2 type arguments expected for list, got " .
        shows (length params) . 
        showString ": " .
        shows (printTree typeName) $
        "")
      let [input, output] = params
      inputType <- getType input
      outputType <- getType output
      return (TFun inputType outputType)
    

assertType :: TCExpr -> [Type] -> TCM Type
assertType expr expectedTypes = (getExprType expr) >>= 
    (\actualType -> if not (elem actualType expectedTypes) 
                    then throwError $ showString "expression " . shows (printTree expr) . showString " expected to be one of types: '" . shows expectedTypes . showString "' but is of type: '" . shows actualType $ "'" 
                    else pure actualType)

getExprType :: TCExpr -> TCM Type
getExprType x = case x of
  EAdd _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ESub _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EMul _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EDiv _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ENotEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\_ -> (assertType expr1 [TInteger, TBool])) >>= (\_ -> pure TInteger)
  ELt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ELtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EInt _ n -> pure TInteger
  ETrue _ -> pure TBool
  EFalse _ -> pure TBool
  EVar _ ident -> getVariableType ident
  ECons _ x xs -> (getExprType xs) >>=
    (\tailType -> case tailType of
      TList t -> (getExprType x) >>= (\headType -> if headType == t 
                                                   then pure . TList $ headType 
                                                   else throwError $ showString "tail of cons has different type than head, have: " . shows headType . showString " and " . shows t $ "")
      TNil -> TList <$> getExprType x
      _ -> throwError $ showString "tail of cons has to be of List type, is: " . shows tailType $ ""
    )
  ENil _ -> pure TNil
  EList _ [] -> pure TNil
  EList _ (x:xs) -> let 
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
  EIfte _ predicateExpr thenExpr elseExpr -> do
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
  ESemicolon _ stmt expr -> (typecheckStmt stmt) >>= ((flip local) (getExprType expr))
  EFunCall _ funExpr argExprs -> do
    funExprType <- getExprType funExpr
    resultType <- foldl (\t -> (\arg -> do
      funType <- t
      newFunType <- case funType of
                      TFun inputType outputType -> do
                        assertType arg [inputType]
                        return outputType
                      _ -> (throwError $
                          showString "trying to call expression of non-function type " .
                          shows funType .
                          showString ": " .
                          shows (printTree x) $
                          ""
                        )
      return newFunType
      )) (pure funExprType) argExprs
    return resultType
  ELambda _ (AFunctionArgument _ argName argTypeName) expr -> do
    argType <- getType argTypeName
    exprType <- local (withVariableType argName argType) (getExprType expr)
    return (TFun argType exprType)
  EMatch _ expr (y:ys) ->
    let checkMatchClause t matchClause@(MMatchClause _ m body) =
          do
            exprType <- getExprType expr
            matchExpr <- interpretTypeMatchExpression m 
            let maybeIdentTypes = matchTypes matchExpr exprType
            when (maybeIdentTypes == Nothing) (throwError $
              showString "impossible match clause " .
              shows (printTree matchClause) .
              showString " in match expression " .
              shows (printTree x) $
              ""
              )
            let (Just identTypes) = maybeIdentTypes
            matchClauseBodyType <- local (\tenv -> foldl (\tenv -> (\(ident, t) -> withVariableType ident t tenv)) tenv identTypes) (getExprType body)
            return matchClauseBodyType
    in (getExprType expr) >>= (\exprType -> foldl (\acc -> (\matchClause -> do
          matchBodyType <- acc
          nextMatchBodyType <- checkMatchClause exprType matchClause
          when (matchBodyType /= nextMatchBodyType) (throwError $ 
            showString "found expressions of differing types as match expression clause bodies: " . 
            shows (printTree y) . showString " of type " . shows matchBodyType .
            showString " and " . 
            shows (printTree matchClause) . showString " of type " . shows nextMatchBodyType .
            showString " in match expression " .
            shows (printTree x) $
            ""
            )
          return matchBodyType
          )) (checkMatchClause exprType y) ys)

typecheckStmt :: TCStmt -> TCM (TEnv -> TEnv)
typecheckStmt stmt = case stmt of
  SDeclVar _ ident expr -> (withVariableType ident) <$> (getExprType expr)
  SDeclFun _ fName args expr -> (\t -> withVariableType fName t) <$>
                                    do
                                      argsParsed <- foldr (\(AFunctionArgument _ name argType) -> (\argsParsed -> do
                                        argsParsed <- argsParsed
                                        argType <- getType argType
                                        return ((name, argType) : argsParsed)
                                        )) (pure []) args
                                      bodyType <- local (\tenv -> (foldl (\tenv -> (\(argName, argType) -> withVariableType argName argType tenv)) tenv argsParsed)) (getExprType expr)
                                      functionType <- pure (foldr (\(argName, argType) -> (\bodyType -> TFun argType bodyType)) bodyType argsParsed)
                                      return functionType
 
interpretTypeMatchExpression :: TCExpr -> TCM CaptureExpression
interpretTypeMatchExpression x = case x of
  EInt _ n -> pure . CaptureInteger $ n
  ETrue _ -> pure . CaptureBool $ True
  EFalse _ -> pure . CaptureBool $ False
  EVar _ ident -> pure . CaptureVariable $ ident
  ECons _ head tail -> (liftM2 CaptureCons) (interpretTypeMatchExpression head) (interpretTypeMatchExpression tail)
  ENil _ -> pure . CaptureList $ []
  EList _ exprs -> CaptureList <$> (foldr (\expr -> (\list -> list >>= (\list -> (\expr -> expr : list) <$> (interpretTypeMatchExpression expr)))) (pure []) exprs)
  _ -> throwError "invalid match expression"

-- TODO This has to return ExceptT on duplicate identifier
matchTypes :: CaptureExpression -> Type -> Maybe [(Ident, Type)]
matchTypes pattern t = case (pattern, t) of
  (CaptureVariable ident, t) -> Just $ [(ident, t)]
  (CaptureInteger x, TInteger) -> Just $ []
  (CaptureBool x, TBool) -> Just $ []
  (CaptureCons x xs, TList elementType) -> (matchTypes x elementType) >>= (\list -> (\rest -> concat [list, rest]) <$> (matchTypes xs (TList elementType)))
  (CaptureList (x:xs), TList elementType) -> (matchTypes x elementType) >>= (\list -> (\rest -> concat [list, rest] ) <$> (matchTypes (CaptureList xs) (TList elementType)))
  (CaptureList [], TList _) -> Just []
  (CaptureList [], TNil) -> Just []
  _ -> Nothing

-- Runtime

-- Interpreter Expression
type IExpr = Expr (Maybe (Int, Int))
type IStmt = Stmt (Maybe (Int, Int))
type IMatchClause = MatchClause (Maybe (Int, Int))
data Value = Integer Integer | Bool Bool | List [Value] | Fun IExpr Env

instance Eq Value where
  (Integer x) == (Integer y) = x == y
  (Bool x) == (Bool y) = x == y
  (List x) == (List y) = x == y
  _ == _ = False

instance Show Value where
  show (Integer i) = show i
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List values) = show values
  show (Fun funExpr _) = show (printTree funExpr)

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

getFunction :: Value -> (Ident, IExpr, Env)
getFunction (Fun (ELambda _ (AFunctionArgument _ argName _) bodyExpr) env) = (argName, bodyExpr, env)

data CaptureExpression = CaptureInteger Integer | CaptureBool Bool | CaptureCons CaptureExpression CaptureExpression  | CaptureList [CaptureExpression] | CaptureVariable Ident

interpretMatchExpression :: IExpr -> IM CaptureExpression
interpretMatchExpression x = case x of
  EInt _ n -> pure . CaptureInteger $ n
  ETrue _ -> pure . CaptureBool $ True
  EFalse _ -> pure . CaptureBool $ False
  EVar _ ident -> pure . CaptureVariable $ ident
  ECons _ head tail -> (liftM2 CaptureCons) (interpretMatchExpression head) (interpretMatchExpression tail)
  ENil _ -> pure . CaptureList $ []
  EList _ exprs -> CaptureList <$> (foldr (\expr -> (\list -> list >>= (\list -> (\expr -> expr : list) <$> (interpretMatchExpression expr)))) (pure []) exprs)
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

interpretExpr :: IExpr -> IM Value
interpretExpr x = case x of
  EAdd _ expr0 expr1  -> binaryOp add expr0 expr1
  ESub _ expr0 expr1  -> binaryOp sub expr0 expr1
  EMul _ expr0 expr1  -> binaryOp multiply expr0 expr1
  EDiv _ expr0 expr1  -> binaryOp divide expr0 expr1
  EEq _ expr0 expr1 -> binaryOp eq expr0 expr1
  ENotEq _ expr0 expr1 -> binaryOp notEq expr0 expr1
  ELt _ expr0 expr1 -> binaryOp lt expr0 expr1
  EGt _ expr0 expr1 -> binaryOp gt expr0 expr1
  ELtEq _ expr0 expr1 -> binaryOp ltEq expr0 expr1
  EGtEq _ expr0 expr1 -> binaryOp gtEq expr0 expr1
  EInt _ n -> pure . Integer $ n
  ETrue _ -> pure . Bool $ True
  EFalse _ -> pure . Bool $ False
  EVar _ ident -> getVariable ident
  ECons _ x xs -> (interpretExpr xs) >>= (\(List xs) -> (\x -> List (x:xs)) <$> (interpretExpr x))
  ENil _ -> pure . List $ []
  EList _ exprs -> List <$> (foldr (\expr -> (\rList -> rList >>= (\list -> (\val -> val : list) <$> (interpretExpr expr)))) (pure []) exprs)
  EIfte _ predicateExpr thenExpr elseExpr ->
    let
      predicate = interpretExpr predicateExpr
      branch = predicate >>= (\value -> case value of
        Bool b -> pure $ if b then thenExpr else elseExpr
        v -> throwError $ showString "invalid predicate in if expression: " . shows v $ ""
        )
    in
      branch >>= interpretExpr
  ESemicolon _ stmt expr -> (interpretStmt stmt) >>= ((flip local) (interpretExpr expr))
  EFunCall _ funExpr argExprs ->
    let 
      handleSingleLevel argExpr (Fun (ELambda Nothing (AFunctionArgument Nothing argName _) bodyExpr) env) =
        let 
          funcParams = do
            argVal <- (interpretExpr argExpr)
            return (argName, argVal, bodyExpr, env)
          callFunc (argName, argVal, bodyExpr, env) = local (\ _ -> withVariable argName argVal env) (interpretExpr bodyExpr)
        in funcParams >>= callFunc
    in foldl (\f -> (\expr -> (f >>= handleSingleLevel expr))) (interpretExpr funExpr) argExprs
  ELambda _ (AFunctionArgument _ argName argType) expr -> Fun (ELambda Nothing (AFunctionArgument Nothing argName argType) expr) <$> ask
  EMatch _ expr clauses ->
    let 
      combine :: Value -> IMatchClause -> Maybe ([(Ident, Value)], IExpr) -> IM (Maybe ([(Ident, Value)], IExpr))
      combine value (MMatchClause _ capture body) acc = case acc of
        Just g -> pure . Just $ g
        Nothing -> (liftM (\captured -> (captured, body))) <$> ((\captureExpr -> match captureExpr value) <$> (interpretMatchExpression capture))
      handleClauses :: [IMatchClause] -> Value -> IM (Maybe ([(Ident, Value)], IExpr))
      handleClauses clauses value =
        foldl (\acc -> (\clause -> acc >>= (\acc -> combine value clause acc))) (pure Nothing) clauses
      capture = (interpretExpr expr) >>= (handleClauses clauses) 
      handleMatch match = case match of
        Just (captured, body) -> local (\env -> foldl (\env -> (\(name, value) -> withVariable name value env)) env captured) (interpretExpr body)
        Nothing -> throwError "Unexhaustive match."
    in capture >>= handleMatch

interpretStmt :: IStmt -> IM (Env -> Env)
interpretStmt stmt = case stmt of
  SDeclVar _ ident expr -> (withVariable ident) <$> (interpretExpr expr)
  SDeclFun _ fName argNames expr -> 
    let
      makeFun env = 
        let
          recEnv = withVariable fName recFun env
          recFun = Fun (foldr (\(AFunctionArgument _ argName argType) -> (\expr -> (ELambda Nothing (AFunctionArgument Nothing argName argType) expr))) expr argNames) recEnv
        in recFun
    in (withVariable fName) <$> (makeFun <$> ask)

calc s =
  let parserOut = pExpr (myLexer s)
  in case parserOut of
    Bad err -> Left (show err)
    Ok expr ->
      let
        outType = runIdentity (runExceptT (runReaderT (getExprType expr) startTEnv))
      in case outType of
        Left e -> Left ("Type Error: " ++ e)
        Right typeName ->
          let 
            out = runIdentity (runExceptT (runReaderT (interpretExpr expr) startEnv))
          in case out of
            Left e -> Left ("Runtime Error: " ++ e)
            Right val -> Right val