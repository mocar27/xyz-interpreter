module TypeChecker.TypeChecker where

import ParserLexer.AbsXyzGrammar as AbsXyzGrammar
import Data.Map                  as Map
-- -- import qualified Data.List                 as List
-- -- import qualified Data.Maybe                as Maybe
-- -- import qualified Data.Either               as Either
import qualified Control.Monad   as Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

-- | Environment mapping variable names to types.
type Env = Map.Map String Type

-- | The type of the type checker.
type TypeChecker a = ReaderT Env (ExceptT String (State Int)) a

-- | Run the type checker.
runTypeChecker :: AbsXyzGrammar.Program -> Either String ()
runTypeChecker program = evalState (runExceptT (runReaderT (typeCheckProgram program) Map.empty)) 0

-- | Type check a program.
typeCheckProgram :: AbsXyzGrammar.Program -> TypeChecker ()
typeCheckProgram (AbsXyzGrammar.MyProgram _ stmts) = typeCheckStmts stmts

-- | Type check a list of statements.
typeCheckStmts :: [AbsXyzGrammar.Stmt] -> TypeChecker ()
typeCheckStmts [] = return ()
typeCheckStmts (stmt : stmts) = do
  typeCheckStmt stmt
  typeCheckStmts stmts

-- | Type check a statement.
typeCheckStmt :: AbsXyzGrammar.Stmt -> TypeChecker ()
typeCheckStmt (AbsXyzGrammar.Empty _) = return ()
typeCheckStmt (AbsXyzGrammar.Decl _ t items) = do
  typeCheckType t
  typeCheckItems items
typeCheckStmt (AbsXyzGrammar.Assign _ _ _) = return ()
typeCheckStmt (AbsXyzGrammar.Ret _ e) = do
  typeCheckExpr e
  return ()
typeCheckStmt (AbsXyzGrammar.VoidRet _) = return ()
typeCheckStmt (AbsXyzGrammar.If _ e block) = do
  typeCheckExpr e
  typeCheckBlock block
typeCheckStmt (AbsXyzGrammar.IfElse _ e block1 block2) = do
    typeCheckExpr e
    typeCheckBlock block1
    typeCheckBlock block2
typeCheckStmt (AbsXyzGrammar.While _ e block) = do
    typeCheckExpr e
    typeCheckBlock block
typeCheckStmt (AbsXyzGrammar.FunctionDef _ t i args block) = do
    typeCheckType t
    typeCheckArgs args
    typeCheckBlock block
typeCheckStmt (AbsXyzGrammar.StmtExp _ e) = do
    typeCheckExpr e
    return ()

-- | Type check a list of items.
typeCheckItems :: [AbsXyzGrammar.Item] -> TypeChecker ()
typeCheckItems [] = return ()
typeCheckItems (item : items) = do
  typeCheckItem item
  typeCheckItems items

-- | Type check an item.
typeCheckItem :: AbsXyzGrammar.Item -> TypeChecker ()
typeCheckItem (AbsXyzGrammar.NoInit _ _) = return ()
typeCheckItem (AbsXyzGrammar.Init _ _ e) = do
  typeCheckExpr e
  return ()

-- | Type check a block.
typeCheckBlock :: AbsXyzGrammar.Block -> TypeChecker ()
typeCheckBlock (AbsXyzGrammar.StmtBlock _ stmts) = typeCheckStmts stmts

-- | Type check a list of arguments.
typeCheckArgs :: [AbsXyzGrammar.Arg] -> TypeChecker ()
typeCheckArgs [] = return ()
typeCheckArgs (arg : args) = do
  typeCheckArg arg
  typeCheckArgs args

-- | Type check an argument.
typeCheckArg :: AbsXyzGrammar.Arg -> TypeChecker ()
typeCheckArg (AbsXyzGrammar.ArgVal _ t _) = typeCheckType t
typeCheckArg (AbsXyzGrammar.ArgRef _ t _) = typeCheckType t

-- | Type check a type.
typeCheckType :: AbsXyzGrammar.Type -> TypeChecker ()
typeCheckType (AbsXyzGrammar.Integer _) = return ()
typeCheckType (AbsXyzGrammar.String _) = return ()
typeCheckType (AbsXyzGrammar.Boolean _) = return ()
typeCheckType (AbsXyzGrammar.Void _) = return ()
typeCheckType (AbsXyzGrammar.Function _ t ts) = do
  typeCheckType t
  typeCheckTypes ts

-- | Type check a list of types.
typeCheckTypes :: [AbsXyzGrammar.Type] -> TypeChecker ()
typeCheckTypes [] = return ()
typeCheckTypes (t : ts) = do
  typeCheckType t
  typeCheckTypes ts

-- | Type check an expression.
typeCheckExpr :: AbsXyzGrammar.Expr -> TypeChecker ()
typeCheckExpr (AbsXyzGrammar.ExpVar _ _) = return ()
typeCheckExpr (AbsXyzGrammar.ExpLitInt _ _) = return ()
typeCheckExpr (AbsXyzGrammar.ExpString _ _) = return ()
typeCheckExpr (AbsXyzGrammar.ExpLitTrue _) = return ()
typeCheckExpr (AbsXyzGrammar.ExpLitFalse _) = return ()
typeCheckExpr (AbsXyzGrammar.ExpApp _ _ es) = do
  typeCheckExprs es
  return ()
typeCheckExpr (AbsXyzGrammar.ExpNeg _ e) = do
    typeCheckExpr e
    return ()
typeCheckExpr (AbsXyzGrammar.ExpNot _ e) = do
    typeCheckExpr e
    return ()
typeCheckExpr (AbsXyzGrammar.ExpMul _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return ()
typeCheckExpr (AbsXyzGrammar.ExpAdd _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return ()
typeCheckExpr (AbsXyzGrammar.ExpRel _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return ()
typeCheckExpr (AbsXyzGrammar.ExpAnd _ e1 e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return ()
typeCheckExpr (AbsXyzGrammar.ExpOr _ e1 e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return ()
typeCheckExpr (AbsXyzGrammar.ExpLambda _ args t b) = do
    typeCheckArgs args
    typeCheckType t
    typeCheckBlock b
    return ()

-- | Type check a list of expressions.
typeCheckExprs :: [AbsXyzGrammar.Expr] -> TypeChecker ()
typeCheckExprs [] = return ()
typeCheckExprs (e : es) = do
  typeCheckExpr e
  typeCheckExprs es
