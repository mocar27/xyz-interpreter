module TypeChecker.TypeChecker where

import TypeChecker.Utils
import ParserLexer.AbsXyzGrammar

import Prelude                  hiding ( foldr )

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

-- | Type check a function block.
typeCheckFunctionBlock :: TType -> FunBlock -> TypeChecker ()  -- TODO
typeCheckFunctionBlock t (FnBlock _ stmts rtrn) = do
  env <- get
  typeCheckStmts stmts
  put env

-- | Type check a return statement.
typeCheckReturn :: Rtrn -> TypeChecker () -- TODO
typeCheckReturn (Ret _ e) = do
  _ <- typeCheckExpr e
  return ()

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
    $ throwError $ "Type mismatch: attempt to assign type " ++ show actualType ++ " to type " ++ show expectedType

typeCheckStmt (If _ e block) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean ()) 
    $ throwError $ "If condition is " ++ show conditionType ++ ", expected was either Boolean ()"
  env <- get
  typeCheckBlock block
  put env

typeCheckStmt (IfElse _ e block1 block2) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean ()) 
    $ throwError $ "If condition is " ++ show conditionType ++ ", expected was either Boolean ()"
  envIf <- get
  typeCheckBlock block1
  put envIf
  envElse <- get
  typeCheckBlock block2
  put envElse

typeCheckStmt (While _ e block) = do
  conditionType <- typeCheckExpr e
  unless (conditionType == Boolean ()) 
    $ throwError $ "While condition is " ++ show conditionType ++ ", expected was either Boolean ()"
  env <- get
  typeCheckBlock block
  put env

typeCheckStmt (FunctionDef _ t i args block) = do -- TODO (If function is not in Env, but passes type check, add it to Env)
  let functionType = omitPosition t
  addFunction i functionType args
  -- typeCheckArgs args
  typeCheckFunctionBlock functionType block
  -- modify (\env -> foldr (\arg env' -> Map.insert (getArgName arg) (getArgType arg) env') env args)
  -- withStateT (\env -> foldr (\arg env' -> Map.insert (getArgName arg) (getArgType arg) env') env args) (typeCheckFunctionBlock functionType block)

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
  let variableName = getNameFromIdent var
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
typeCheckType (RefInteger _) = return ()
typeCheckType (RefString _) = return ()
typeCheckType (RefBoolean _) = return ()
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

typeCheckExpr (ExpApp _ _ es) = do  -- TODO (probably getting type of function from Env and comparing to variable it's assigned to)
  typeCheckExprs es
  return $ Integer ()

typeCheckExpr (ExpNeg _ e) = do
  varType <- typeCheckExpr e
  unless (varType == Integer ()) 
    $ throwError $ "Negation Error: expected type was Integer (), but got " ++ show varType ++ " instead."
  return $ Integer ()

typeCheckExpr (ExpNot _ e) = do
  varType <- typeCheckExpr e
  unless (varType == Boolean ())
    $ throwError $ "Negation Error: expected type was Boolean (), but got " ++ show varType ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpMul _ e1 _ e2) = do
  varType1 <- typeCheckExpr e1
  varType2 <- typeCheckExpr e2
  unless (varType1 == Integer () && varType2 == Integer ())
    $ throwError $ "Multiplication Error: expected types were Integer (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Integer ()

typeCheckExpr (ExpAdd _ e1 _ e2) = do
  varType1 <- typeCheckExpr e1
  varType2 <- typeCheckExpr e2
  if varType1 == String () && varType2 == String () then return $ String ()
  else if varType1 == Integer () && varType2 == Integer () then return $ Integer ()
  else
    throwError $ "Addition Error: expected types were either both Integer () or String (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."

typeCheckExpr (ExpRel _ e1 op e2) = do
  varType1 <- typeCheckExpr e1
  varType2 <- typeCheckExpr e2
  unless (varType1 == varType2)
    $ throwError $ "Relational Error: cannot do relation operation on mismatch types: " ++ show varType1 ++ " and " ++ show varType2
  return $ Boolean ()

typeCheckExpr (ExpAnd _ e1 e2) = do
  varType1 <- typeCheckExpr e1
  varType2 <- typeCheckExpr e2
  unless (varType1 == Boolean () && varType2 == Boolean ())
    $ throwError $ "And Error: expected types were Boolean (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpOr _ e1 e2) = do
  varType1 <- typeCheckExpr e1
  varType2 <- typeCheckExpr e2
  unless (varType1 == Boolean () && varType2 == Boolean ())
    $ throwError $ "Or Error: expected types were Boolean (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpLambda _ args t b) = do -- TODO
    typeCheckArgs args
    let funType = omitPosition t
    typeCheckFunctionBlock funType b
    return $ Integer ()
