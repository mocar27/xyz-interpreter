module TypeChecker.TypeChecker where

import TypeChecker.Utils
import ParserLexer.AbsXyzGrammar

import Prelude                  hiding ( map )

import Data.Map                  as Map
import Data.Functor.Identity     ( runIdentity )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

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
typeCheckFunctionBlock :: TType -> FunBlock -> TypeChecker ()
typeCheckFunctionBlock t (FnBlock _ stmts rtrn) = do
  typeCheckStmts stmts
  typeCheckReturn t rtrn

-- | Type check a return statement.
typeCheckReturn :: TType -> Rtrn -> TypeChecker ()
typeCheckReturn eT (Ret _ e) = do
  rtrnType <- typeCheckExpr e
  unless (rtrnType == eT)
    $ throwError $ "Return type mismatch: Function is type " ++ show eT ++ ", but return is type " ++ show rtrnType

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
  let tempType = omitPositionRef t
  let declType = getTypeFromType tempType
  typeCheckItems declType items
  addVariables declType items

typeCheckStmt (Assign _ var e) = do
  expectedType <- getVarFromEnv var
  let expectedTypeFromRef = getTypeFromRef expectedType
  actualType <- typeCheckExpr e
  unless (actualType == expectedType || actualType == expectedTypeFromRef)
    $ throwError $ "Type mismatch: attempt to assign type " ++ show actualType ++ " to type " ++ show expectedType

typeCheckStmt (If _ e blck) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFromType tempType
  unless (conditionType == Boolean ())
    $ throwError $ "If condition is " ++ show conditionType ++ ", expected was Boolean ()"
  env <- get
  typeCheckBlock blck
  put env

typeCheckStmt (IfElse _ e blck1 blck2) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFromType tempType
  unless (conditionType == Boolean ())
    $ throwError $ "If condition is " ++ show conditionType ++ ", expected was Boolean ()"
  envIf <- get
  typeCheckBlock blck1
  put envIf
  envElse <- get
  typeCheckBlock blck2
  put envElse

typeCheckStmt (While _ e blck) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFromType tempType
  unless (conditionType == Boolean ())
    $ throwError $ "While condition is " ++ show conditionType ++ ", expected was Boolean ()"
  env <- get
  typeCheckBlock blck
  put env

typeCheckStmt (FunctionDef _ t idnt args blck) = do
  let functionType = omitPosition t
  env <- get
  forM_ args $ \a -> case a of
    ArgVal _ t _ -> modify (Map.insert (getArgName a) (omitPosition t))
    ArgRef _ t _ -> modify (Map.insert (getArgName a) (omitPositionRef t))
  typeCheckFunctionBlock functionType blck
  put env
  addFunction idnt functionType args

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
typeCheckItem eT (Init _ var e) = do
  let variableName = getNameFromIdent var
  actualType <- typeCheckExpr e
  unless (actualType == eT)
    $ throwError $ "Type mismatch variable " ++ show variableName ++ ": " ++ show actualType ++ " cannot be assigned to " ++ show eT

-- | Type check a list of arguments.
typeCheckArgs :: [TType] -> [TType] -> TypeChecker ()
typeCheckArgs [] [] = return ()
typeCheckArgs [] _ = throwError "Function application Error: too many arguments"
typeCheckArgs _ [] = throwError "Function application Error: too few arguments"
typeCheckArgs (fArg : funArgs) (gArg : givenArgs) = do
  typeCheckArg fArg gArg
  typeCheckArgs funArgs givenArgs

-- | Type check an argument.
typeCheckArg :: TType -> TType -> TypeChecker ()
typeCheckArg expected given = do 
  let expectedType = getTypeFromType expected
  let givenType = getTypeFromType given 
  unless (expectedType == givenType)
  $ throwError $ "Function application Error: expected type " ++ show expected ++ ", but got " ++ show given ++ " instead."

-- | Type check a list of types.
typeCheckTypes :: [Type] -> TypeChecker ()
typeCheckTypes [] = return ()
typeCheckTypes (t : ts) = do
  typeCheckType t
  typeCheckTypes ts

-- | Type check a type.
typeCheckType :: Type -> TypeChecker TType
typeCheckType (Integer _) = return $ Integer ()
typeCheckType (String _) = return $ String ()
typeCheckType (Boolean _) = return $ Boolean ()
typeCheckType (RefInteger _) = return $ RefInteger ()
typeCheckType (RefString _) = return $ RefString ()
typeCheckType (RefBoolean _) = return $ RefBoolean ()
typeCheckType (Function _ retType argTypes) = do
  rtrnT <- typeCheckType retType
  mapM_ typeCheckType argTypes
  let argTypes' = fmap omitPositionRef argTypes
  return $ Function () rtrnT argTypes'

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

typeCheckExpr (ExpApp _ f args) = do
  function <- getFunctionFromEnv f
  functionArgsTypes <- getFunctionArgTypesFromEnv f
  rtrnType <- getFunctionRetTypeFromEnv f
  unless (length functionArgsTypes == length args)
    $ throwError $ "Function application Error: expected " ++ show (length functionArgsTypes) ++ " arguments, but got " ++ show (length args) ++ " instead."
  givenArgs <- mapM typeCheckExpr args
  typeCheckArgs functionArgsTypes givenArgs
  return rtrnType 

typeCheckExpr (ExpNeg _ e) = do
  tempType <- typeCheckExpr e
  let varType = getTypeFromType tempType
  unless (varType == Integer ())
    $ throwError $ "Negation Error: expected type was Integer (), but got " ++ show varType ++ " instead."
  return $ Integer ()

typeCheckExpr (ExpNot _ e) = do
  tempType <- typeCheckExpr e
  let varType = getTypeFromType tempType
  unless (varType == Boolean ())
    $ throwError $ "Negation Error: expected type was Boolean (), but got " ++ show varType ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpMul _ e1 _ e2) = do
  tempType1 <- typeCheckExpr e1
  tempType2 <- typeCheckExpr e2
  let varType1 = getTypeFromType tempType1
  let varType2 = getTypeFromType tempType2
  unless (varType1 == Integer () && varType2 == Integer ())
    $ throwError $ "Multiplication Error: expected types were Integer (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Integer ()

typeCheckExpr (ExpAdd _ e1 _ e2) = do
  tempType1 <- typeCheckExpr e1
  tempType2 <- typeCheckExpr e2
  let varType1 = getTypeFromType tempType1
  let varType2 = getTypeFromType tempType2
  if varType1 == String () && varType2 == String () then return $ String ()
  else if varType1 == Integer () && varType2 == Integer () then return $ Integer ()
  else
    throwError $ "Addition Error: expected types were either both Integer () or String (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."

typeCheckExpr (ExpRel _ e1 op e2) = do
  tempType1 <- typeCheckExpr e1
  tempType2 <- typeCheckExpr e2
  let varType1 = getTypeFromType tempType1
  let varType2 = getTypeFromType tempType2
  unless (varType1 == varType2)
    $ throwError $ "Relational Error: cannot do relation operation on mismatch types: " ++ show varType1 ++ " and " ++ show varType2
  return $ Boolean ()

typeCheckExpr (ExpAnd _ e1 e2) = do
  tempType1 <- typeCheckExpr e1
  tempType2 <- typeCheckExpr e2
  let varType1 = getTypeFromType tempType1
  let varType2 = getTypeFromType tempType2
  unless (varType1 == Boolean () && varType2 == Boolean ())
    $ throwError $ "And Error: expected types were Boolean (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpOr _ e1 e2) = do
  tempType1 <- typeCheckExpr e1
  tempType2 <- typeCheckExpr e2
  let varType1 = getTypeFromType tempType1
  let varType2 = getTypeFromType tempType2
  unless (varType1 == Boolean () && varType2 == Boolean ())
    $ throwError $ "Or Error: expected types were Boolean (), but got " ++ show varType1 ++ " and " ++ show varType2 ++ " instead."
  return $ Boolean ()

typeCheckExpr (ExpLambda _ args t blck) = do
  let functionType = omitPosition t
  env <- get
  forM_ args $ \a -> case a of
    ArgVal _ t _ -> modify (Map.insert (getArgName a) (omitPosition t))
    ArgRef _ t _ -> modify (Map.insert (getArgName a) (omitPositionRef t))
  typeCheckFunctionBlock functionType blck
  put env

  rtrnT <- typeCheckType t
  let argTypes' = fmap getArgType args
  return $ Function () rtrnT argTypes'
