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

getFromInsideOfExprFunctor expr = case expr of
  EAdd a _ _ -> a
  ESub a _ _ -> a
  EMul a _ _ -> a
  EDiv a _ _ -> a
  EEq a _ _ -> a
  ENotEq a _ _ -> a
  ELt a _ _ -> a
  EGt a _ _ -> a
  ELtEq a _ _ -> a
  EGtEq a _ _ -> a
  EInt a _ -> a
  ENegInt a _ -> a
  EOr a _ _ -> a
  EAnd a _ _ -> a
  ENot a _ -> a
  ETrue a -> a
  EFalse a -> a
  ELambda a _ _ -> a
  EVar a ident -> a
  EFunCall a _ _ -> a
  ECons a _ _ -> a
  EList a _ -> a
  ENil a -> a
  EIfte a _ _ _ -> a
  ESemicolon a _ _ -> a
  EMatch a _ _ -> a

getExprLine expr = let (Just (line, _)) = getFromInsideOfExprFunctor expr in line

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
type TCM = ReaderT TEnv (ExceptT (Int, TCError) Identity)

data TCError = TCUnknownVariable Ident | 
              TCUnknownSimpleType Ident | 
              TCUnknownPolymorphicType Ident | 
              TCUnexpectedPolymorphicTypeArgCount Int Int (TypeName ()) | 
              TCInvalidExpressionType (Expr ()) [Type] Type |
              TCInvalidMatchExpression (Expr ()) |
              TCImpossibleMatchClause (MatchClause ()) (Expr ()) |
              TCNonFunctionCall (Expr ()) Type
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

clearM m = (\_ -> ()) <$> m

startTEnv = TEnv { variable_types = Map.empty }
withVariableType :: Ident -> Type -> TEnv -> TEnv
withVariableType ident t env = env { variable_types = Map.insert ident t (variable_types env) }

getVariableType :: TCExpr -> TCM Type
getVariableType expr@(EVar _ ident) = (Map.lookup) <$> (pure ident) <*> (asks variable_types) >>= 
  (\result -> case result of 
    Just t -> pure t
    Nothing -> throwError (getExprLine expr, (TCUnknownVariable ident))
    )

getType :: TCTypeName -> TCM Type -- TODO: add custom type handling
getType typeName = case typeName of
  TSimpleTypeName (Just (line, _)) (Ident name) -> case name of
    "Int" -> pure TInteger
    "Bool" -> pure TBool
    _ -> (throwError (line, (TCUnknownSimpleType (Ident name))))
  TPolymorphicTypeName (Just (line, _)) (Ident name) params -> case name of
    "List" -> do
      when ((length params) /= 1) (throwError (line, (TCUnexpectedPolymorphicTypeArgCount 1 (length params) (clearM typeName))))
      elementType <- getType (head params)
      return (TList elementType)
    "Fun" -> do
      when ((length params) /= 2) (throwError (line, (TCUnexpectedPolymorphicTypeArgCount 2 (length params) (clearM typeName))))
      let [input, output] = params
      inputType <- getType input
      outputType <- getType output
      return (TFun inputType outputType)
    _ -> (throwError (line, (TCUnknownPolymorphicType (Ident name))))
    

assertType :: TCExpr -> [Type] -> TCM Type
assertType expr expectedTypes = (getExprType expr) >>= 
    (\actualType -> 
      let
        containsListType = (elem True) $ (\x -> 
          case x of
            TList _ -> True
            _ -> False) <$> expectedTypes
        expectedTypesWithNil = if containsListType then TNil : expectedTypes else expectedTypes
      in
        if not (elem actualType expectedTypesWithNil) 
        then throwError (getExprLine expr, TCInvalidExpressionType (clearM expr) expectedTypesWithNil actualType) 
        else pure actualType)

getExprType :: TCExpr -> TCM Type
getExprType x = case x of
  EAdd _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  ESub _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EMul _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EDiv _ expr0 expr1  -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TInteger)
  EEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\leftType -> (assertType expr1 [leftType])) >>= (\rightType -> pure TBool) -- TODO: Fix eq typecheck
  ENotEq _ expr0 expr1 -> (assertType expr0 [TInteger, TBool]) >>= (\leftType -> (assertType expr1 [leftType])) >>= (\rightType -> pure TBool)
  ELt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TBool)
  EGt _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TBool)
  ELtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TBool)
  EGtEq _ expr0 expr1 -> (assertType expr0 [TInteger]) >>= (\_ -> (assertType expr1 [TInteger])) >>= (\_ -> pure TBool)
  EInt _ n -> pure TInteger
  ENegInt _ n -> pure TInteger
  ETrue _ -> pure TBool
  EFalse _ -> pure TBool
  EVar _ ident -> getVariableType x
  ECons _ x xs -> do
    headType <- getExprType x
    tailType <- assertType xs [(TList headType)]
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
                      _ -> throwError (getExprLine x, TCNonFunctionCall (clearM x) funType)
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
            when (maybeIdentTypes == Nothing) (throwError (getExprLine x, (TCImpossibleMatchClause (clearM matchClause) (clearM x))))
            let (Just identTypes) = maybeIdentTypes
            matchClauseBodyType <- local (\tenv -> foldl (\tenv -> (\(ident, t) -> withVariableType ident t tenv)) tenv identTypes) (getExprType body)
            return matchClauseBodyType
    in (getExprType expr) >>= (\exprType -> foldl (\acc -> (\matchClause@(MMatchClause _ _ body) -> do
          matchBodyType <- acc
          nextMatchBodyType <- checkMatchClause exprType matchClause
          when (matchBodyType /= nextMatchBodyType) (throwError (getExprLine x, (TCInvalidExpressionType (clearM body) [matchBodyType] nextMatchBodyType)))
          return matchBodyType
          )) (checkMatchClause exprType y) ys)

typecheckStmt :: TCStmt -> TCM (TEnv -> TEnv)
typecheckStmt stmt = case stmt of
  SDeclVar _ ident expr -> (withVariableType ident) <$> (getExprType expr)
  SDeclFun _ fName args returnType expr -> (\t -> withVariableType fName t) <$>
                                    do
                                      argsParsed <- foldr (\(AFunctionArgument _ name argType) -> (\argsParsed -> do
                                        argsParsed <- argsParsed
                                        argType <- getType argType
                                        return ((name, argType) : argsParsed)
                                        )) (pure []) args
                                      returnTypeParsed <- getType returnType
                                      let functionType = foldr (\(argName, argType) -> (\bodyType -> TFun argType bodyType)) returnTypeParsed argsParsed
                                      bodyType <- local (\tenv -> (foldl (\tenv -> (\(argName, argType) -> withVariableType argName argType tenv)) (withVariableType fName functionType tenv) argsParsed)) (getExprType expr)
                                      when (bodyType /= returnTypeParsed) (throwError (getExprLine expr, TCInvalidExpressionType (clearM expr) [returnTypeParsed] bodyType))
                                      return functionType
 
interpretTypeMatchExpression :: TCExpr -> TCM CaptureExpression
interpretTypeMatchExpression x = case x of
  EInt _ n -> pure . CaptureInteger $ n
  ENegInt _ n -> pure . CaptureInteger $ (-n)
  ETrue _ -> pure . CaptureBool $ True
  EFalse _ -> pure . CaptureBool $ False
  EVar _ ident -> pure . CaptureVariable $ ident
  ECons _ head tail -> (liftM2 CaptureCons) (interpretTypeMatchExpression head) (interpretTypeMatchExpression tail)
  ENil _ -> pure . CaptureList $ []
  EList _ exprs -> CaptureList <$> (foldr (\expr -> (\list -> list >>= (\list -> (\expr -> expr : list) <$> (interpretTypeMatchExpression expr)))) (pure []) exprs)
  _ -> throwError (getExprLine x, TCInvalidMatchExpression (clearM x))

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
  ENegInt _ n -> pure . CaptureInteger $ (-n)
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
  ENegInt _ n -> pure . Integer $ (-n)
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
  SDeclFun _ fName argNames _ expr -> 
    let
      makeFun env = 
        let
          recEnv = withVariable fName recFun env
          recFun = Fun (foldr (\(AFunctionArgument _ argName argType) -> (\expr -> (ELambda Nothing (AFunctionArgument Nothing argName argType) expr))) expr argNames) recEnv
        in recFun
    in (withVariable fName) <$> (makeFun <$> ask)

data CalcError = CalcParserError String | CalcTCError (Int, TCError) | CalcIError IError deriving (Eq)

instance Show CalcError where
  show (CalcParserError str) = "Parser Error: " ++ str
  show (CalcTCError (line, err)) = "Type Check Error in line " ++ (show line) ++ ": " ++ (show err)
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