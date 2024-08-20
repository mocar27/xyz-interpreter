{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluator.Utils where

import ParserLexer.AbsXyzGrammar

import Prelude                   as C

import Data.Map                  as Map

import Control.Monad.State
import Control.Monad.Except

-- | Types
type Err = String
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
  | VFun ([Arg], FunBlock, Type) Env
  | PrintInteger
  | PrintString
  | PrintBoolean
  deriving (C.Show)

instance Ord Value where
  (VInt a) < (VInt b) = a < b
  (VStr a) < (VStr b) = a < b
  (VBool a) < (VBool b) = a < b
  (VInt a) <= (VInt b) = a <= b
  (VStr a) <= (VStr b) = a <= b
  (VBool a) <= (VBool b) = a <= b
  (VInt a) > (VInt b) = a > b
  (VStr a) > (VStr b) = a > b
  (VBool a) > (VBool b) = a > b
  (VInt a) >= (VInt b) = a >= b
  (VStr a) >= (VStr b) = a >= b
  (VBool a) >= (VBool b) = a >= b

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VStr a) == (VStr b) = a == b
  (VBool a) == (VBool b) = a == b
  _ == _ = False

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

addVariableLocToEnv :: Var -> Loc -> Evaluator ()
addVariableLocToEnv var loc = modifyEnv (Map.insert var loc)

storeVariableValue :: Loc -> Value -> Evaluator ()
storeVariableValue loc val = modifyStore (Map.insert loc val)

modifyStore :: (Store -> Store) -> Evaluator ()
modifyStore f = modify (\(env, store) -> (env, f store))

modifyEnv :: (Env -> Env) -> Evaluator ()
modifyEnv f = modify (\(env, store) -> (f env, store))

getEnv :: Evaluator Env
getEnv = do 
  (env, _) <- get
  return env

putEnv :: Env -> Evaluator ()
putEnv env = modify (\(_, store) -> (env, store))

getStore :: Evaluator Store
getStore = do
  (_, store) <- get
  return store

putStore :: Store -> Evaluator ()
putStore store = modify (\(env, _) -> (env, store))

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

getArgLoc :: Expr -> Evaluator Loc
getArgLoc (ExpVar _ var) = getLocOfVar (getNameFromIdent var)
getArgLoc _ = error "Expected variable"

setArg :: Arg -> Value -> Evaluator ()
setArg (ArgVal _ _ (Ident name)) val = do
  (_, s) <- get
  let loc = newLoc s
  addVariableLocToEnv name loc
  storeVariableValue loc val 
setArg (ArgRef _ _ (Ident name)) (VLoc loc) = addVariableLocToEnv name loc
setArg _ _ = error "Expected variable"

throwPosError :: BNFC'Position -> String -> Evaluator a
throwPosError (Just (l, c)) msg = throwError $ show l ++ ":" ++ show c ++ " " ++ msg
throwPosError Nothing msg = throwError msg

-- | Helper functions
getNameFromIdent :: Ident -> String
getNameFromIdent (Ident var) = var

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

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x : _) = x
