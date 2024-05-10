module TypeChecker.TypeChecker where

import TypeChecker.Utils
import ParserLexer.AbsXyzGrammar

import Data.Map                  as Map
import Data.Functor.Identity     ( runIdentity )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Typeable             (Typeable, typeOf)

-- | Run the type checker.
runTypeChecker :: Program -> Either String ()
runTypeChecker program = runIdentity . runExceptT . (`evalStateT` initialEnv) $ typeCheckProgram program

-- | Type check a program.
typeCheckProgram :: Program -> TypeChecker ()
typeCheckProgram (MyProgram _ stmts) = typeCheckStmts stmts

-- | Type check a block.
typeCheckBlock :: Block -> TypeChecker ()
typeCheckBlock (StmtBlock _ stmts) = typeCheckStmts stmts

-- | Type check a list of statements.
typeCheckStmts :: [Stmt] -> TypeChecker ()
typeCheckStmts [] = return ()
typeCheckStmts (stmt : stmts) = do
  typeCheckStmt stmt
  typeCheckStmts stmts

-- | Type check a statement.
typeCheckStmt :: Stmt -> TypeChecker ()
typeCheckStmt (Empty _) = return ()

typeCheckStmt (Decl _ t items) = do
  let declType = omitPosition t
  typeCheckItems declType items
  addVariables declType items

typeCheckStmt (Assign i v e) = do
  expectedType <- getVarFromEnv v
  actualType <- typeCheckExpr e
  unless (actualType == expectedType) 
    $ throwError $ "Type mismatch: Attempt to assign type: " ++ show actualType ++ " to type: " ++ show expectedType
  
typeCheckStmt (Ret _ e) = do -- TODO
  _ <- typeCheckExpr e
  return ()
typeCheckStmt (VoidRet _) = return () -- TODO

typeCheckStmt (If _ e block) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean () || conditionType == Integer ()) 
    $ throwError $ "If condition is: " ++ show conditionType ++ ", expected was either: Boolean () or Integer ()"
  env <- get
  typeCheckBlock block
  put env

typeCheckStmt (IfElse _ e block1 block2) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean () || conditionType == Integer ()) 
    $ throwError $ "If condition is: " ++ show conditionType ++ ", expected was either: Boolean () or Integer ()"
  envIf <- get
  typeCheckBlock block1
  put envIf
  envElse <- get
  typeCheckBlock block2
  put envElse

typeCheckStmt (While _ e block) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean () || conditionType == Integer ()) 
    $ throwError $ "While condition is: " ++ show conditionType ++ ", expected was either: Boolean () or Integer ()"
  env <- get
  typeCheckBlock block
  put env

typeCheckStmt (FunctionDef _ t i args block) = do -- TODO (If function is not in Env, but passes type check, add it to Env)
    -- let declType = omitPosition t
  typeCheckArgs args
  typeCheckBlock block  

typeCheckStmt (StmtExp _ e) = do
  _ <- typeCheckExpr e
  return ()

-- | Type check a list of items.
typeCheckItems :: TType -> [Item] -> TypeChecker ()
typeCheckItems _ [] = return ()
typeCheckItems eT (item : items) = do
  typeCheckItem eT item
  typeCheckItems eT items

-- | Type check an item.
typeCheckItem :: TType -> Item -> TypeChecker ()
typeCheckItem _ (NoInit _ _) = return ()
typeCheckItem eT (Init p var e) = do
  let variableName = getVarFromIdent var
  actualType <- typeCheckExpr e
  unless (actualType == eT) 
    $ throwError $ "Type mismatch variable " ++ show variableName ++ ": " ++ show actualType ++ " cannot be assigned to " ++ show eT

-- | Type check a list of arguments.
typeCheckArgs :: [Arg] -> TypeChecker ()  -- TODO
typeCheckArgs [] = return ()
typeCheckArgs (arg : args) = do
  typeCheckArg arg
  typeCheckArgs args

-- | Type check an argument.
typeCheckArg :: Arg -> TypeChecker ()     -- TODO
typeCheckArg (ArgVal _ t _) = do 
  typeCheckType t
typeCheckArg (ArgRef _ t _) = do 
  typeCheckType t

-- | Type check a list of types.          -- TODO
typeCheckTypes :: [Type] -> TypeChecker ()
typeCheckTypes [] = return ()
typeCheckTypes (t : ts) = do
  typeCheckType t
  typeCheckTypes ts

-- | Type check a type.
typeCheckType :: Type -> TypeChecker ()   -- TODO
typeCheckType (Integer _) = return ()
typeCheckType (String _) = return ()
typeCheckType (Boolean _) = return ()
typeCheckType (Void _) = return ()
typeCheckType (Function _ retType argTypes) = do
  typeCheckType retType
  mapM_ typeCheckType argTypes

-- | Type check a list of expressions.
typeCheckExprs :: [Expr] -> TypeChecker ()
typeCheckExprs [] = return ()
typeCheckExprs (e : es) = do
  typeCheckExpr e
  typeCheckExprs es

-- | Type check an expression.
typeCheckExpr :: Expr -> TypeChecker TType
typeCheckExpr (ExpVar _ var) = getVarFromEnv var
typeCheckExpr (ExpLitInt _ _) = return $ Integer ()
typeCheckExpr (ExpString _ _) = return $ String ()
typeCheckExpr (ExpLitTrue _) = return $ Boolean ()
typeCheckExpr (ExpLitFalse _) = return $ Boolean ()
typeCheckExpr (ExpApp _ _ es) = do
  typeCheckExprs es
  return $ Integer ()
typeCheckExpr (ExpNeg _ e) = do
    typeCheckExpr e
    return $ Integer ()
typeCheckExpr (ExpNot _ e) = do
    typeCheckExpr e
    return $ Integer ()
typeCheckExpr (ExpMul _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return $ Integer ()
typeCheckExpr (ExpAdd _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return $ Integer ()
typeCheckExpr (ExpRel _ e1 _ e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return $ Integer ()
typeCheckExpr (ExpAnd _ e1 e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return $ Integer ()
typeCheckExpr (ExpOr _ e1 e2) = do
    typeCheckExpr e1
    typeCheckExpr e2
    return $ Integer ()
typeCheckExpr (ExpLambda _ args t b) = do
    typeCheckArgs args
    -- omitPosition t
    typeCheckBlock b
    return $ Integer ()
