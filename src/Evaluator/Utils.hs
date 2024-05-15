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
  | VString String
  | VBool Bool
  | VLoc Loc
  | VFun (Arg, [Stmt], Type) Env
  deriving (C.Eq, C.Show)

initialEnv :: Env
initialEnv = Map.empty

initialStore :: Store
initialStore = Map.empty
