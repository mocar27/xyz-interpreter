module TypeChecker.Utils where

import ParserLexer.AbsXyzGrammar

import Data.Map                  as Map
import Data.Functor.Identity     ( Identity, runIdentity )

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
addVariables :: [Item] -> TType -> TypeChecker ()
addVariables [] _ = return ()
addVariables ((Init p v e) : items) t = do
  let variableName = getVarFromIdent v
  modify (Map.insert variableName t)
  addVariables items t

-- | Get the type of a variable from the environment.
getVarFromEnv :: Ident -> TypeChecker TType
getVarFromEnv (Ident var) = do
  env <- get
  case Map.lookup var env of
    Just t  -> return t
    Nothing -> throwError $ "Variable " ++ var ++ " not declared"

getVarFromIdent :: Ident -> String
getVarFromIdent (Ident var) = var

omitPosition :: Type' a -> TType
omitPosition (Integer _) = Integer ()
omitPosition (String _) = String ()
omitPosition (Boolean _) = Boolean ()
omitPosition (Void _) = Void ()
omitPosition (Function _ retType argTypes) = Function () (omitPosition retType) (fmap omitPosition argTypes)