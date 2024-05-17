module Evaluator.Utils where

import ParserLexer.AbsXyzGrammar

import Prelude                   as C

import Data.Map                  as Map

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
-- data Fun = Fun ([Arg], FunBlock, Type) Env | PrintInteger | PrintString | PrintBoolean
  -- deriving (C.Eq, C.Show)

data Value
  = VInt Integer
  | VStr String
  | VBool Bool
  | VLoc Loc
  | VFun ([Arg], FunBlock, Type) Env
  | PrintInteger
  | PrintString
  | PrintBoolean
  deriving (C.Eq, C.Show)

-- | Environment and Store mappings and functions
initialEnv :: Env
initialEnv = Map.fromList [
  ("printInteger", 0),
  ("printString", 1),
  ("printBoolean", 2)
  ]

initialStore :: Store
initialStore = Map.fromList [
  (0, PrintInteger),
  (1, PrintString),
  (2, PrintBoolean)
  ]

newLoc :: Store -> Integer
newLoc = toInteger . size

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

getValueFromVal :: Value -> Either (Either String Integer) Bool
getValueFromVal (VInt i) = Left (Right i)
getValueFromVal (VStr s) = Left (Left s)
getValueFromVal (VBool b) = Right b
getValueFromVal _ = error "Expected Integer, String or Boolean value"

getIntFromVal :: Value -> Integer
getIntFromVal (VInt i) = i
getIntFromVal _ = error "Expected Integer value"

getStringFromVal :: Value -> String
getStringFromVal (VStr s) = s
getStringFromVal _ = error "Expected String value"

getBoolFromVal :: Value -> Bool
getBoolFromVal (VBool b) = b
getBoolFromVal _ = error "Expected Boolean value"

isInteger :: Value -> Bool
isInteger (VInt _) = True
isInteger _ = False

isString :: Value -> Bool
isString (VStr _) = True
isString _ = False

isBoolean :: Value -> Bool
isBoolean (VBool _) = True
isBoolean _ = False

getArgName :: Arg -> String
getArgName (ArgVal _ _ name) = getNameFromIdent name
getArgName (ArgRef _ _ name) = getNameFromIdent name

defaultValue :: Type -> Value
defaultValue (Integer _) = VInt 0
defaultValue (String _) = VStr ""
defaultValue (Boolean _) = VBool False
defaultValue _ = error "Cannot create default value for non-primitive type"
