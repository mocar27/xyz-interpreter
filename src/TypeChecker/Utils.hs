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
  let variableName = getVarFromIdent v
  modify (Map.insert variableName t)
  addVariables t items
addVariables t ((Init _ v _) : items) = do
  let variableName = getVarFromIdent v
  modify (Map.insert variableName t)
  addVariables t items

-- | Get the type of a variable from the environment.
getVarFromEnv :: Ident -> TypeChecker TType
getVarFromEnv (Ident var) = do
  env <- get
  case Map.lookup var env of
    Just t  -> return t
    Nothing -> throwError $ "Variable " ++ var ++ " not declared"

getVarFromIdent :: Ident -> String
getVarFromIdent (Ident var) = var

getOperationType :: RelOp' BNFC'Position -> String
getOperationType (LThan _) = "<"
getOperationType (Leq _) = "<="
getOperationType (GThan _) = ">"
getOperationType (Geq _) = ">="
getOperationType (Eq _) = "=="
getOperationType (NEq _) = "!="

getArgName :: Arg -> String
getArgName (ArgVal _ _ name) = getVarFromIdent name
getArgName (ArgRef _ _ name) = getVarFromIdent name

getArgType :: Arg -> TType
getArgType (ArgVal _ t _) = omitPosition t
getArgType (ArgRef _ t _) = omitPosition t

omitPosition :: Type' BNFC'Position -> TType
omitPosition (Integer _) = Integer ()
omitPosition (String _) = String ()
omitPosition (Boolean _) = Boolean ()
omitPosition (Void _) = Void ()
omitPosition (Function _ retType argTypes) = Function () (omitPosition retType) (fmap omitPosition argTypes)
