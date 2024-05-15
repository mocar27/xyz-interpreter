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

initialEnv :: Env
initialEnv = Map.empty

initialStore :: Store
initialStore = Map.empty

getNameFromIdent :: Ident -> String
getNameFromIdent (Ident var) = var

getArgName :: Arg -> String
getArgName (ArgVal _ _ name) = getNameFromIdent name
getArgName (ArgRef _ _ name) = getNameFromIdent name

defaultVal :: Type -> Value
defaultVal (Integer _) = VInt 0
defaultVal (String _) = VStr ""
defaultVal (Boolean _) = VBool False

getIntFromVal :: Value -> Integer
getIntFromVal (VInt i) = i
