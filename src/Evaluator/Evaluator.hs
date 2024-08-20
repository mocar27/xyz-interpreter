{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Evaluator.Evaluator where

import Evaluator.Utils
import ParserLexer.AbsXyzGrammar

import Prelude                    hiding ( foldr )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- | Run the evaluator.
runEvaluator :: Program -> IO (Either Err ())
runEvaluator program = runExceptT $ evalStateT (evalProgram program) (initialEnv, initialStore)

-- | Evaluate a program.
evalProgram :: Program -> Evaluator ()
evalProgram (MyProgram p stmts) = do
  _ <- evalStmts stmts
  _ <- evalExpr (ExpApp p (Ident "main") [])
  return ()

-- | Evaluate a block.
evalBlock :: Block -> Evaluator ()
evalBlock (StmtBlock _ stmts) = do 
  env <- getEnv
  evalStmts stmts
  putEnv env

-- | Evaluate a function block.
evalFBlock :: FunBlock -> Evaluator Value
evalFBlock (FnBlock _ stmts rtrn) = do
  evalStmts stmts
  evalReturn rtrn

-- | Evaluate a return statement.
evalReturn :: Rtrn -> Evaluator Value
evalReturn (Ret _ e) = evalExpr e

-- | Evaluate statements.
evalStmts :: [Stmt] -> Evaluator ()
evalStmts [] = return ()
evalStmts (stmt : stmts) = do
  _ <- evalStmt stmt
  evalStmts stmts

evalStmt :: Stmt -> Evaluator ()
evalStmt (Empty _) = return ()

evalStmt (Decl _ t items) = evalItems t items

evalStmt (Assign _ var e) = do
  val <- evalExpr e
  loc <- getLocOfVar (getNameFromIdent var)
  storeVariableValue loc val

evalStmt (If _ e blck) = do
  val <- evalExpr e
  let cond = getBoolFromVal val
  when cond $ evalBlock blck

evalStmt (IfElse _ e blck1 blck2) = do
  val <- evalExpr e
  let cond = getBoolFromVal val
  if cond then do
    evalBlock blck1
  else do
    evalBlock blck2

evalStmt (While p e blck) = do
  val <- evalExpr e
  let cond = getBoolFromVal val
  when cond $ evalBlock blck >> evalStmt (While p e blck)

evalStmt (FunctionDef _ t ident args blck) = do
  s <- getStore
  let var = getNameFromIdent ident
  let newL = newLoc s
  addVariableLocToEnv var newL
  env <- getEnv
  let fun = VFun (args, blck, t) env
  storeVariableValue newL fun

evalStmt (StmtExp _ e) = do
  _ <- evalExpr e
  return ()

-- | Evaluate items.
evalItems :: Type -> [Item] -> Evaluator ()
evalItems _ [] = return ()
evalItems t ((NoInit _ v) : items) = do
  s <- getStore
  let newL = newLoc s
  addVariableLocToEnv (getNameFromIdent v) newL
  storeVariableValue newL (defaultValue t)
  evalItems t items
evalItems t ((Init _ v e) : items) = do
  val <- evalExpr e
  s <- getStore
  let newL = newLoc s
  addVariableLocToEnv (getNameFromIdent v) newL
  storeVariableValue newL val
  evalItems t items

-- | Evaluate arguments.
evalExprArg :: BNFC'Position -> Arg -> Expr -> Evaluator Value
evalExprArg _ (ArgVal _ _ _) e = evalExpr e
evalExprArg _ (ArgRef _ _ _) (ExpVar _ (Ident var))= do
  loc <- getLocOfVar var
  return $ VLoc loc
evalExprArg p _ _ = throwPosError p "Function call error: Expected reference but got value (probably const) instead."

setFunArgsAndEnv :: BNFC'Position -> [Arg] -> [Expr] -> Env -> Evaluator ()
setFunArgsAndEnv p fargs es fenv = do
  vals <- zipWithM (evalExprArg p) fargs es
  modifyEnv (const fenv)
  forM_ (zip fargs vals) $ uncurry setArg

-- | Evaluate an expression.
evalExpr :: Expr -> Evaluator Value
evalExpr (ExpVar _ var) = getValue (getNameFromIdent var)

evalExpr (ExpLitInt _ i) = return $ VInt i
evalExpr (ExpString _ s) = return $ VStr s
evalExpr (ExpLitTrue _) = return $ VBool True
evalExpr (ExpLitFalse _) = return $ VBool False

evalExpr (ExpApp p ident args) = do
  env <- getEnv
  let funName = getNameFromIdent ident
  function <- getValue funName
  case function of
    VFun (fargs, blck, _) fenv -> do
      setFunArgsAndEnv p fargs args fenv
      rtrnValue <- evalFBlock blck
      putEnv env
      return rtrnValue
    PrintInteger -> do
      val <- evalExpr (myHead args)
      liftIO $ print $ getIntFromVal val
      return $ VInt 0
    PrintString -> do
      val <- evalExpr (myHead args)
      liftIO $ print $ getStringFromVal val
      return $ VStr ""
    PrintBoolean -> do
      val <- evalExpr (myHead args)
      liftIO $ print $ getBoolFromVal val
      return $ VBool False
    _ -> throwPosError p "Expected function."

evalExpr (ExpNeg _ e) = do
  val <- evalExpr e
  let i = getIntFromVal val
  return $ VInt (-i)

evalExpr (ExpNot _ e) = do
  val <- evalExpr e
  let b = getBoolFromVal val
  return $ VBool (not b)

evalExpr (ExpMul p e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let i1 = getIntFromVal val1
  let i2 = getIntFromVal val2
  case op of
    Multi _ -> return $ VInt (i1 * i2)
    Div _ -> do
      if i2 == 0 then throwPosError p "Multiplication error: Division by zero."
      else return $ VInt (i1 `div` i2)
    Mod _ -> return $ VInt (i1 `mod` i2)

evalExpr (ExpAdd p e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let v1 = isInteger val1
  let v2 = isInteger val2
  case op of
    Plus _ -> if v1 && v2 then return $ VInt (getIntFromVal val1 + getIntFromVal val2)
              else if isString val1 && isString val2 then return $ VStr (getStringFromVal val1 ++ getStringFromVal val2)
              else throwPosError p "Addition Error: expected both types either Integer or String."
    Minus _ -> if v1 && v2 then return $ VInt (getIntFromVal val1 - getIntFromVal val2)
               else throwPosError p "Subtraction Error: expected types were Integers."

evalExpr (ExpRel _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  case op of
    LThan _ -> return $ VBool (val1 < val2)
    Leq _ -> return $ VBool (val1 <= val2)
    GThan _ -> return $ VBool (val1 > val2)
    Geq _ -> return $ VBool (val1 >= val2)
    Eq _ -> return $ VBool (val1 == val2)
    NEq _ -> return $ VBool (val1 /= val2)

evalExpr (ExpAnd _ e1 e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let b1 = getBoolFromVal val1
  let b2 = getBoolFromVal val2
  return $ VBool (b1 && b2)

evalExpr (ExpOr _ e1 e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let b1 = getBoolFromVal val1
  let b2 = getBoolFromVal val2
  return $ VBool (b1 || b2)

evalExpr (ExpLambda _ args t blck) = do
  VFun (args, blck, t) <$> getEnv
