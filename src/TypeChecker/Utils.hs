module TypeChecker.Utils where

import ParserLexer.AbsXyzGrammar

import Data.Map                  as Map
import Data.Functor.Identity     ( Identity )

import Control.Monad.State
import Control.Monad.Except

-- | Types
type TType = Type' ()
type Env = Map.Map String TType
type TypeChecker a = StateT Env (ExceptT String Identity) a

-- | Environment mapping variable names to types.
initialEnv :: Env
initialEnv = Map.empty

-- Function to add a variable to the global environment.
addVariables :: TType -> [Item] -> TypeChecker ()
addVariables _ [] = return ()
addVariables t ((NoInit _ v) : items) = do
  let variableName = getNameFromIdent v
  modify (Map.insert variableName t)
  addVariables t items
addVariables t ((Init _ v _) : items) = do
  let variableName = getNameFromIdent v
  modify (Map.insert variableName t)
  addVariables t items

addFunction :: Ident -> TType -> [Arg] -> TypeChecker ()
addFunction (Ident name) retType args = do
  let argTypes = fmap getArgType args
  modify (Map.insert name (Function () retType argTypes))

getVarFromEnv :: Ident -> TypeChecker TType
getVarFromEnv (Ident var) = do
  env <- get
  case Map.lookup var env of
    Just (RefString _) -> return $ String ()
    Just (RefInteger _) -> return $ Integer ()
    Just (RefBoolean _) -> return $ Boolean ()
    Just t  -> return t
    Nothing -> throwError $ "Variable " ++ var ++ " not declared"

getNameFromIdent :: Ident -> String
getNameFromIdent (Ident var) = var

getOperationType :: RelOp' BNFC'Position -> String
getOperationType (LThan _) = "<"
getOperationType (Leq _) = "<="
getOperationType (GThan _) = ">"
getOperationType (Geq _) = ">="
getOperationType (Eq _) = "=="
getOperationType (NEq _) = "!="

getArgName :: Arg -> String
getArgName (ArgVal _ _ name) = getNameFromIdent name
getArgName (ArgRef _ _ name) = getNameFromIdent name

getArgType :: Arg -> TType
getArgType (ArgVal _ t _) = omitPosition t
getArgType (ArgRef _ t _) = omitPositionRef t

getFunctionFromEnv :: Ident -> TypeChecker TType
getFunctionFromEnv (Ident name) = do
  env <- get
  case Map.lookup name env of
    Just t  -> return t
    Nothing -> throwError $ "Function with name " ++ name ++ " not declared"

getFunctionRetTypeFromEnv :: Ident -> TypeChecker TType
getFunctionRetTypeFromEnv (Ident name) = do
  env <- get
  case Map.lookup name env of
    Just (Function _ retType _) -> return retType
    Just _ -> throwError $ "Variable with name " ++ name ++ " is not a function"
    Nothing -> throwError $ "Function with name " ++ name ++ " not declared"

getFunctionArgTypesFromEnv :: Ident -> TypeChecker [TType]
getFunctionArgTypesFromEnv (Ident name) = do
  env <- get
  case Map.lookup name env of
    Just (Function _ _ argTypes) -> return argTypes
    Just _ -> throwError $ "Variable with name " ++ name ++ " is not a function"
    Nothing -> throwError $ "Function with name " ++ name ++ " not declared"

omitPositionRef :: Type' BNFC'Position -> TType
omitPositionRef (Integer _) = RefInteger ()
omitPositionRef (String _) = RefString ()
omitPositionRef (Boolean _) = RefBoolean ()
omitPositionRef t = omitPosition t

omitPosition :: Type' BNFC'Position -> TType
omitPosition (Integer _) = Integer ()
omitPosition (String _) = String ()
omitPosition (Boolean _) = Boolean ()
omitPosition (RefInteger _) = RefInteger ()
omitPosition (RefString _) = RefString ()
omitPosition (RefBoolean _) = RefBoolean ()
omitPosition (Function _ retType argTypes) = Function () (omitPosition retType) (fmap omitPositionRef argTypes)
