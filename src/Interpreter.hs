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

type TCExpr = Expr (Maybe (Int, Int))
type TCStmt = Stmt (Maybe (Int, Int))
type TCTypeName = TypeName (Maybe (Int, Int))
type TCMatchClause = MatchClause (Maybe (Int, Int))

data TEnv = TEnv { variable_types :: Map.Map Ident Type } deriving (Show)
type TCM = ReaderT TEnv (ExceptT TCError Identity)

data TCError = TCUnknownVariable Ident | 
              TCUnknownSimpleType Ident | 
              TCUnknownPolymorphicType Ident | 
              TCUnexpectedPolymorphicTypeArgCount Int Int TCTypeName | 
              TCInvalidExpressionType TCExpr [Type] Type |
              TCInvalidMatchExpression TCExpr |
              TCImpossibleMatchClause TCMatchClause TCExpr |
              TCNonFunctionCall TCExpr Type
              deriving (Eq)

instance Show TCError where
  show (TCUnknownVariable (Ident str)) = "unknown variable used: " ++ str
  show (TCUnknownSimpleType (Ident str)) = "unknown simple type name: " ++ str
  show (TCUnknownPolymorphicType (Ident str)) = "unknown polymorphic type name: " ++ str
  show (TCUnexpectedPolymorphicTypeArgCount want actual typeName) = "exactly " ++ (show want) ++ " type argument expected for list, got " ++ (show actual) ++ ": " ++ (show (printTree typeName)) 
  show (TCInvalidExpressionType expr want actual) = "expression " ++ (show (printTree expr)) ++ " expected to be one of types: '" ++ (show want) ++ "' but is of type: '" ++ (show actual) ++ "'"
  show (TCInvalidMatchExpression expr) = "invalid match expression: " ++ (show (printTree expr)) 
  show (TCImpossibleMatchClause matchClause expr) = "impossible match clause " ++ (show (printTree matchClause)) ++ " in match expression " ++ (show (printTree expr))
  show (TCNonFunctionCall expr t) = "trying to call expression of non-function type " ++ (show t) ++ ": " ++ (show (printTree expr))

startTEnv = TEnv { variable_types = Map.empty }
withVariableType :: Ident -> Type -> TEnv -> TEnv
withVariableType ident t env = env { variable_types = Map.insert ident t (variable_types env) }

getVariableType :: Ident -> TCM Type
getVariableType ident = (Map.lookup) <$> (pure ident) <*> (asks variable_types) >>= 
  (\result -> case result of 
    Just t -> pure t
    Nothing -> throwError (TCUnknownVariable ident)
    )

getType :: TCTypeName -> TCM Type -- TODO: add custom type handling
getType typeName = case typeName of
  TSimpleTypeName _ (Ident name) -> case name of
    "Int" -> pure TInteger
    "Bool" -> pure TBool
    _ -> (throwError (TCUnknownSimpleType (Ident name)))
  TPolymorphicTypeName _ (Ident name) params -> case name of
    "List" -> do
      when ((length params) /= 1) (throwError (TCUnexpectedPolymorphicTypeArgCount 1 (length params) typeName))
      elementType <- getType (head params)
      return (TList elementType)
    "Fun" -> do
      when ((length params) /= 2) (throwError (TCUnexpectedPolymorphicTypeArgCount 2 (length params) typeName))
      let [input, output] = params
      inputType <- getType input
      outputType <- getType output
      return (TFun inputType outputType)
    _ -> (throwError (TCUnknownPolymorphicType (Ident name)))
    

assertType :: TCExpr -> [Type] -> TCM Type
assertType expr expectedTypes = (getExprType expr) >>= 
    (\actualType -> if not (elem actualType expectedTypes) 
                    then throwError (TCInvalidExpressionType expr expectedTypes actualType) 
                    else pure actualType)

getExprType :: TCExpr -> TCM Type
getExprType x = case x of
  EAdd _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ESub _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EMul _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EDiv _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\leftType -> (assertType expr1 [leftType])) >>= (\rightType -> pure rightType) -- TODO: Fix eq typecheck
  ENotEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\leftType -> (assertType expr1 [leftType])) >>= (\rightType -> pure rightType)
  ELt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ELtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EGtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EInt _ n -> pure TInteger
  ETrue _ -> pure TBool
  EFalse _ -> pure TBool
  EVar _ ident -> getVariableType ident
  ECons _ x xs -> do
    headType <- getExprType x
    tailType <- assertType xs [(TList headType), TNil]
    return (TList headType)
  ENil _ -> pure TNil
  EList _ [] -> pure TNil
  EList _ (x:xs) -> let 
    checkUniformType headType xs =
      foldl (\acc -> (\curExpr -> do
        accType <- acc
        assertType curExpr [accType]
        return accType
      )) headType xs
    in TList <$> checkUniformType (getExprType x) xs
  EIfte _ predicateExpr thenExpr elseExpr -> do
    predicateType <- getExprType predicateExpr
    assertType predicateExpr [TBool]
    thenType <- getExprType thenExpr
    assertType elseExpr [thenType]
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
                      _ -> throwError (TCNonFunctionCall x funType)
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
            when (maybeIdentTypes == Nothing) (throwError (TCImpossibleMatchClause matchClause x))
            let (Just identTypes) = maybeIdentTypes
            matchClauseBodyType <- local (\tenv -> foldl (\tenv -> (\(ident, t) -> withVariableType ident t tenv)) tenv identTypes) (getExprType body)
            return matchClauseBodyType
    in (getExprType expr) >>= (\exprType -> foldl (\acc -> (\matchClause@(MMatchClause _ _ body) -> do
          matchBodyType <- acc
          nextMatchBodyType <- checkMatchClause exprType matchClause
          when (matchBodyType /= nextMatchBodyType) (throwError (TCInvalidExpressionType body [matchBodyType] nextMatchBodyType))
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
  _ -> throwError (TCInvalidMatchExpression x)

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
data Env = Env { variables :: Map.Map Ident Value } deriving (Show)
type IM = ReaderT Env (ExceptT IError Identity)
type IExpr = Expr (Maybe (Int, Int))
type IStmt = Stmt (Maybe (Int, Int))
type IMatchClause = MatchClause (Maybe (Int, Int))
data Value = Integer Integer | Bool Bool | List [Value] | Fun IExpr Env
data IError = IErrUnexpected String |
              IErrDivisionByZero |
              IErrUnexhaustiveMatch
              deriving (Eq)

instance Show IError where
  show (IErrUnexpected str) = "unexpected error: " ++ str
  show IErrDivisionByZero = "division by zero"
  show IErrUnexhaustiveMatch = "unexhaustive pattern match"

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

add :: Value -> Value -> IM Value
add (Integer v1) (Integer v2) = pure . Integer $ (v1 + v2)
sub :: Value -> Value -> IM Value
sub (Integer v1) (Integer v2) = pure . Integer $ (v1 - v2)
multiply :: Value -> Value -> IM Value
multiply (Integer v1) (Integer v2) = pure . Integer $ (v1 * v2)
divide :: Value -> Value -> IM Value
divide (Integer v1) (Integer 0) = throwError IErrDivisionByZero
divide (Integer v1) (Integer v2) = pure . Integer $ (v1 `div` v2)

eq :: Value -> Value -> IM Value
eq (Integer v1) (Integer v2) = pure . Bool $ (v1 == v2)
eq (Bool v1) (Bool v2) = pure . Bool $ (v1 == v2)
eq (List (v1:v1s)) (List (v2:v2s)) = eq v1 v2 >>= (\(Bool ok) -> if ok then eq (List v1s) (List v2s) else pure . Bool $ False)
eq (List []) (List []) = pure . Bool $ True
eq (List _) (List _) = pure . Bool $ False
notEq v1 v2 = Bool . (\(Bool b) -> not b) <$> (eq v1 v2)

lt :: Value -> Value -> IM Value
lt (Integer v1) (Integer v2) = pure . Bool $ (v1 < v2)
gt :: Value -> Value -> IM Value
gt (Integer v1) (Integer v2) = pure . Bool $ (v1 > v2)
ltEq :: Value -> Value -> IM Value
ltEq (Integer v1) (Integer v2) = pure . Bool $ (v1 <= v2)
gtEq :: Value -> Value -> IM Value
gtEq (Integer v1) (Integer v2) = pure . Bool $ (v1 >= v2)

startEnv = Env { variables = Map.empty }
withVariable :: Ident -> Value -> Env -> Env
withVariable ident val env = env { variables = Map.insert ident val (variables env) }

getVariable :: Ident -> IM Value
getVariable ident@(Ident str) = (Map.lookup) <$> (pure ident) <*> (asks variables) >>= 
  (\result -> case result of 
    Just value -> pure value
    Nothing -> throwError (IErrUnexpected $ showString "unknown variable used: " . showString str $ "")
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
  _ -> throwError (IErrUnexpected "invalid match expression")

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
      branch = (\(Bool b) -> if b then thenExpr else elseExpr) <$> predicate
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
        Nothing -> throwError IErrUnexhaustiveMatch
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

data CalcError = CalcParserError String | CalcTCError TCError | CalcIError IError deriving (Eq)

instance Show CalcError where
  show (CalcParserError str) = "Parser Error: " ++ str
  show (CalcTCError err) = "Type Check Error: " ++ (show err)
  show (CalcIError err) = "Runtime Error: " ++ (show err)

calc s =
  let parserOut = pExpr (myLexer s)
  in case parserOut of
    Bad err -> Left (CalcParserError (show err))
    Ok expr ->
      let
        outType = runIdentity (runExceptT (runReaderT (getExprType expr) startTEnv))
      in case outType of
        Left e -> Left (CalcTCError e)
        Right typeName ->
          let 
            out = runIdentity (runExceptT (runReaderT (interpretExpr expr) startEnv))
          in case out of
            Left e -> Left (CalcIError e)
            Right val -> Right val