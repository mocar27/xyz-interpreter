module Evaluator.Evaluator where

import Evaluator.Utils
import ParserLexer.AbsXyzGrammar

import Data.Map                  as Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- | Run the evaluator.
runEvaluator :: Program -> IO (Either Err ExitCode)
runEvaluator program = runExceptT $ evalStateT (evalProgram program) (initialEnv, initialStore)

-- | Evaluate a program.
evalProgram :: Program -> Evaluator ExitCode
evalProgram (MyProgram p stmts) = do
  _ <- evalStmts stmts
  evalExpr (ExpApp p (Ident "main") [])

evalBlock :: Block -> Evaluator ExitCode -- todo
evalBlock (StmtBlock _ stmts) = do
  (env, _) <- get
  evalStmts stmts
  put env
  return $ VInt 0

evalFBlock :: FunBlock -> Evaluator ExitCode -- todo
evalFBlock (FnBlock _ stmts rtrn) = do
  env <- get
  evalStmts stmts
  put env
  evalStmt rtrn

evalStmts :: [Stmt] -> Evaluator () -- todo
evalStmts [] = return ()
evalStmts (stmt : stmts) = do
  evalStmt stmt
  evalStmts stmts

evalStmt :: Stmt -> Evaluator ExitCode -- todo
evalStmt (Empty _) = return $ VInt 0

evalStmt (Decl _ t items) = do
  evalItems t items
  return $ VInt 0

evalStmt (Assign _ var e) = do
  loc <- getLocOfVar (getNameFromIdent var)
  val <- evalExpr e
  storeVariableValue loc val
  return $ VInt 0

-- evalStmt (Print _ e) = do
--     val <- evalExpr e
--     liftIO $ print val
--     return VInt 0

evalStmt (If _ e stmt) = do
  val <- evalExpr e
  case val of
    VBool True -> evalBlock stmt
    VBool False -> return $ VInt 0

evalStmt (IfElse _ e stmt1 stmt2) = do
  val <- evalExpr e
  case val of
    VBool True -> evalStmt stmt1
    VBool False -> evalStmt stmt2

evalStmt (While p e stmt) = do
  val <- evalExpr e
  case val of
    VBool True -> evalStmt stmt >> evalStmt (While p e stmt)
    VBool False -> return $ VInt 0

evalStmt (StmtBlock _ block) = evalBlock block

evalStmt (Ret _ e) = evalExpr e

evalItems :: Type -> [Item] -> Evaluator () -- todo
evalItems _ [] = return ()
evalItems t ((NoInit _ v) : items) = do
  loc <- getLocOfVar (getNameFromIdent v)
  storeVariableValue loc (defaultVal t)
  evalItems t items
evalItems t ((Init _ v e) : items) = do
  loc <- getLocOfVar (getNameFromIdent v)
  val <- evalExpr e
  storeVariableValue loc val
  evalItems t items

-- evalExprArg :: Env -> Expr -> Value -- todo
-- evalExprArg env e = evalStateT (evalExpr e) (env, initialStore)

evalExpr :: Expr -> Evaluator Value -- todo
evalExpr (ExpVar _ var) = do
  loc <- getLocOfVar (getNameFromIdent var)
  getValueFromLoc loc

evalExpr (ExpLitInt _ i) = return $ VInt i
evalExpr (ExpString _ s) = return $ VStr s
evalExpr (ExpLitTrue _) = return $ VBool True
evalExpr (ExpLitFalse _) = return $ VBool False

evalExpr (ExpApp _ ident args) = do
  function <- getValue (getNameFromIdent ident)
  -- env <- get
  -- let argNames = fmap getArgName funArgs
  -- let argVals = fmap (evalExprArg env) args
  -- let newEnv = Map.fromList $ zip argNames argVals
  -- put (newEnv, initialStore)
  -- retVal <- evalStmts funStmts
  -- put env
  -- return retVal
  return $ VInt 0

evalExpr (ExpNeg _ e) = do
  val <- evalExpr e
  case val of
    VInt i -> return $ VInt (-i)

evalExpr (ExpNot _ e) = do
  val <- evalExpr e
  case val of
    VBool b -> return $ VBool (not b)

evalExpr (ExpMul _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  case op of
    Multi _ -> return $ VInt (getIntFromVal val1 * getIntFromVal val2)
    Div _ -> return $ VInt (getIntFromVal val1 `div` getIntFromVal val2)
    Mod _ -> return $ VInt (getIntFromVal val1 `mod` getIntFromVal val2)

evalExpr (ExpAdd _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  case op of
    Plus _ -> case (val1, val2) of
      (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
      (VStr s1, VStr s2) -> return $ VStr (s1 ++ s2)
    Minus _ -> return $ VInt (getIntFromVal val1 - getIntFromVal val2)

evalExpr (ExpRel _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  case op of
    LThan _ -> return $ VBool (getIntFromVal val1 < getIntFromVal val2)
    Leq _ -> return $ VBool (getIntFromVal val1 <= getIntFromVal val2)
    GThan _ -> return $ VBool (getIntFromVal val1 > getIntFromVal val2)
    Geq _ -> return $ VBool (getIntFromVal val1 >= getIntFromVal val2)
    Eq _ -> return $ VBool (val1 == val2)
    NEq _ -> return $ VBool (val1 /= val2)

evalExpr (ExpAnd _ e1 e2) = do
  val1 <- evalExpr e1
  case val1 of
    VBool False -> return $ VBool False
    VBool True -> do
      val2 <- evalExpr e2
      case val2 of
        VBool b -> return $ VBool b

evalExpr (ExpOr _ e1 e2) = do
  val1 <- evalExpr e1
  case val1 of
    VBool True -> return $ VBool True
    VBool False -> do
      val2 <- evalExpr e2
      case val2 of
        VBool b -> return $ VBool b
