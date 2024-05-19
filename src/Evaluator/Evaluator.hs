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
evalBlock (StmtBlock _ stmts) = evalStmts stmts

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
  (env, _) <- get
  let cond = getBoolFromVal val
  when cond $ evalBlock blck
  modify (\(_, st) -> (env, st))

evalStmt (IfElse _ e blck1 blck2) = do
  val <- evalExpr e
  (env, _) <- get
  let cond = getBoolFromVal val
  if cond then do
    evalBlock blck1
    modify (\(_, st) -> (env, st))
  else do
    evalBlock blck2
    modify (\(_, st) -> (env, st))

evalStmt (While p e blck) = do
  val <- evalExpr e
  (env, _) <- get
  let cond = getBoolFromVal val
  when cond $ evalBlock blck >> evalStmt (While p e blck)
  modify (\(_, st) -> (env, st))

evalStmt (FunctionDef _ t ident args blck) = do
  (_, s) <- get
  let var = getNameFromIdent ident
  let newL = newLoc s
  addVariableLocToEnv var newL
  (env, _) <- get
  let fun = VFun (args, blck, t) env
  storeVariableValue newL fun

evalStmt (StmtExp _ e) = do
  _ <- evalExpr e
  return ()

-- | Evaluate items.
evalItems :: Type -> [Item] -> Evaluator ()
evalItems _ [] = return ()
evalItems t ((NoInit _ v) : items) = do
  (_, s) <- get
  let newL = newLoc s
  addVariableLocToEnv (getNameFromIdent v) newL
  storeVariableValue newL (defaultValue t)
  evalItems t items
evalItems t ((Init _ v e) : items) = do
  val <- evalExpr e
  (_, s) <- get
  let newL = newLoc s
  addVariableLocToEnv (getNameFromIdent v) newL
  storeVariableValue newL val
  evalItems t items

-- | Evaluate arguments.
evalExprArg :: Arg -> Expr -> Evaluator Value
evalExprArg (ArgVal _ _ _) e = evalExpr e
evalExprArg (ArgRef _ _ _) (ExpVar _ (Ident var))= do
  loc <- getLocOfVar var
  return $ VLoc loc
evalExprArg _ _ = error "Expected variable"

setFunArgsAndEnv :: [Arg] -> [Expr] -> Env -> Evaluator ()
setFunArgsAndEnv fargs es fenv = do
  vals <- zipWithM evalExprArg fargs es
  modifyEnv (const fenv)
  forM_ (zip fargs vals) $ uncurry setArg

-- | Evaluate an expression.
evalExpr :: Expr -> Evaluator Value
evalExpr (ExpVar _ var) = getValue (getNameFromIdent var)

evalExpr (ExpLitInt _ i) = return $ VInt i
evalExpr (ExpString _ s) = return $ VStr s
evalExpr (ExpLitTrue _) = return $ VBool True
evalExpr (ExpLitFalse _) = return $ VBool False

evalExpr (ExpApp _ ident args) = do
  (env, _) <- get
  let funName = getNameFromIdent ident
  function <- getValue funName
  case function of
    VFun (fargs, blck, _) fenv -> do
      setFunArgsAndEnv fargs args fenv
      rtrnValue <- evalFBlock blck
      modify (\(_, st) -> (env, st))
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
    _ -> error "Expected function"

evalExpr (ExpNeg _ e) = do
  val <- evalExpr e
  let i = getIntFromVal val
  return $ VInt (-i)

evalExpr (ExpNot _ e) = do
  val <- evalExpr e
  let b = getBoolFromVal val
  return $ VBool (not b)

evalExpr (ExpMul _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let i1 = getIntFromVal val1
  let i2 = getIntFromVal val2
  case op of
    Multi _ -> return $ VInt (i1 * i2)
    Div _ -> do
      if i2 == 0 then error "Multiplication error: Division by zero"
      else return $ VInt (i1 `div` i2)
    Mod _ -> return $ VInt (i1 `mod` i2)

evalExpr (ExpAdd _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let v1 = isInteger val1
  let v2 = isInteger val2
  case op of
    Plus _ -> if v1 && v2 then return $ VInt (getIntFromVal val1 + getIntFromVal val2)
              else if isString val1 && isString val2 then return $ VStr (getStringFromVal val1 ++ getStringFromVal val2)
              else error "Addition Error: expected types were Integer () and Integer (), or String () and String (), but got something else instead."
    Minus _ -> if v1 && v2 then return $ VInt (getIntFromVal val1 - getIntFromVal val2)
               else error "Subtraction Error: expected types were Integer () and Integer (), but got something else instead."

evalExpr (ExpRel _ e1 op e2) = do
  val1 <- evalExpr e1
  val2 <- evalExpr e2
  let v1 = getValueFromVal val1
  let v2 = getValueFromVal val2
  case op of
    LThan _ -> return $ VBool (v1 < v2)
    Leq _ -> return $ VBool (v1 <= v2)
    GThan _ -> return $ VBool (v1 > v2)
    Geq _ -> return $ VBool (v1 >= v2)
    Eq _ -> return $ VBool (v1 == v2)
    NEq _ -> return $ VBool (v1 /= v2)

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
  (env, _) <- get
  return $ VFun (args, blck, t) env
