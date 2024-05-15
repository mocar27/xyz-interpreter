module Evaluator.Evaluator where

import Evaluator.Utils
import ParserLexer.AbsXyzGrammar

import Data.Map                  as Map
import Data.Functor.Identity     ( Identity, runIdentity )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- | Run the evaluator.
runEvaluator :: Program -> IO (Either Err ExitCode)
runEvaluator program = runExceptT $ evalStateT (evalProgram program) (initialEnv, initialStore)

evalProgram :: Program -> Evaluator ExitCode
evalProgram (MyProgram p stmts) = do
    throwError "Not implemented"
