module Evaluator.Utils where

import ParserLexer.AbsXyzGrammar

import Prelude                   as C

import Data.Map                  as Map
import Data.Functor.Identity     ( Identity )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- | Types
type Err = String
type ExitCode = Value
type Var = String
type Loc = Integer
type Env = Map.Map Var Loc
type Store = Map.Map Loc Value
type Evaluator a = StateT (Env, Store) (ExceptT String IO) a

-- | Values
data Value
  = VInt Integer
  | VStr String
  | VBool Bool
  | VLoc Loc
  | VFun (Arg, [Stmt], Type) Env
  deriving (C.Eq, C.Show)

-- | Environment and Store mappings and functions
initialEnv :: Env
initialEnv = Map.empty

initialStore :: Store
initialStore = Map.empty

addVariableToEnv :: Var -> Loc -> Evaluator ()
addVariableToEnv var loc = modifyEnv (Map.insert var loc)

storeVariableValue :: Loc -> Value -> Evaluator ()
storeVariableValue loc val = modifyStore (Map.insert loc val)

modifyStore :: (Store -> Store) -> Evaluator ()
modifyStore f = modify (\(env, store) -> (env, f store))

modifyEnv :: (Env -> Env) -> Evaluator ()
modifyEnv f = modify (\(env, store) -> (f env, store))

getValue :: Var -> Evaluator Value
getValue var = do
  loc <- getLocOfVar var
  getValueFromLoc loc

getLocOfVar :: Var -> Evaluator Loc
getLocOfVar var = do
	(env, _) <- get
	case Map.lookup var env of
		Just loc -> return loc
		Nothing -> throwError $ "Variable " ++ var ++ " not declared"

getValueFromLoc :: Loc -> Evaluator Value
getValueFromLoc loc = do
	(_, store) <- get
	case Map.lookup loc store of
		Just val -> return val
		Nothing -> throwError $ "Location " ++ show loc ++ " not found"

-- | Helper functions
getNameFromIdent :: Ident -> String
getNameFromIdent (Ident var) = var

getIntFromVal :: Value -> Integer
getIntFromVal (VInt i) = i

getArgName :: Arg -> String
getArgName (ArgVal _ _ name) = getNameFromIdent name
getArgName (ArgRef _ _ name) = getNameFromIdent name

defaultVal :: Type -> Value
defaultVal (Integer _) = VInt 0
defaultVal (String _) = VStr ""
defaultVal (Boolean _) = VBool False
