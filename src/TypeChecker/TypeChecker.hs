module TypeChecker.TypeChecker where

import TypeChecker.Utils
import ParserLexer.AbsXyzGrammar
import ParserLexer.PrintXyzGrammar ( printTree )

import Data.Map                    as Map
import Data.Functor.Identity       ( runIdentity )

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- | Run the type checker.
runTypeChecker :: Program -> Either Err ()
runTypeChecker program = runIdentity . runExceptT . (`evalStateT` initialEnv) $ typeCheckProgram program

-- | Type check a program.
typeCheckProgram :: Program -> TypeChecker ()
typeCheckProgram (MyProgram _ stmts) = typeCheckStmts stmts

-- | Type check a block.
typeCheckBlock :: Block -> TypeChecker ()
typeCheckBlock (StmtBlock _ stmts) = do 
  env <- get
  typeCheckStmts stmts
  put env

-- | Type check a function block.
typeCheckFunctionBlock :: TType -> FunBlock -> TypeChecker ()
typeCheckFunctionBlock t (FnBlock _ stmts rtrn) = do
  typeCheckStmts stmts
  typeCheckReturn t rtrn

-- | Type check a return statement.
typeCheckReturn :: TType -> Rtrn -> TypeChecker ()
typeCheckReturn eT (Ret p e) = do
  rtrnType <- typeCheckExpr e
  unless (rtrnType == eT) $
    throwPosError p ("Return type mismatch: Function is type " ++ printTree eT ++ ", but return is type " ++ printTree rtrnType)

-- | Type check statements.
typeCheckStmts :: [Stmt] -> TypeChecker ()
typeCheckStmts [] = return ()
typeCheckStmts (stmt : stmts) = do
  typeCheckStmt stmt
  typeCheckStmts stmts

typeCheckStmt :: Stmt -> TypeChecker ()
typeCheckStmt (Empty _) = return ()

typeCheckStmt (Decl _ t items) = do
  let tempType = omitPositionRef t
  let declType = getTypeFrom tempType
  typeCheckItems declType items
  addVariables declType items

typeCheckStmt (Assign p var e) = do
  expectedType <- getVarFromEnv var
  let expectedTypeFromRef = getTypeFrom expectedType
  actualType <- typeCheckExpr e
  unless (actualType == expectedType || actualType == expectedTypeFromRef)
    $ throwPosError p ("Type mismatch: attempt to assign type " ++ printTree actualType ++ " to type " ++ printTree expectedType)

typeCheckStmt (If p e blck) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFrom tempType
  unless (conditionType == Boolean ())
    $ throwPosError p ("If condition is " ++ printTree conditionType ++ ", expected was Boolean")
  typeCheckBlock blck

typeCheckStmt (IfElse p e blck1 blck2) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFrom tempType
  unless (conditionType == Boolean ())
    $ throwPosError p ("If condition is " ++ printTree conditionType ++ ", expected was Boolean")
  typeCheckBlock blck1
  typeCheckBlock blck2

typeCheckStmt (While p e blck) = do
  tempType <- typeCheckExpr e
  let conditionType = getTypeFrom tempType
  unless (conditionType == Boolean ())
    $ throwPosError p ("While condition is " ++ printTree conditionType ++ ", expected was Boolean")
  typeCheckBlock blck

typeCheckStmt (FunctionDef _ t idnt args blck) = do
  let functionType = omitPosition t
  addFunction idnt functionType args
  processFunction t args blck

typeCheckStmt (StmtExp _ e) = do
  _ <- typeCheckExpr e
  return ()

-- | Type check items.
typeCheckItems :: TType -> [Item] -> TypeChecker ()
typeCheckItems _ [] = return ()
typeCheckItems eT (item : items) = do
  typeCheckItem eT item
  typeCheckItems eT items

typeCheckItem :: TType -> Item -> TypeChecker ()
typeCheckItem _ (NoInit _ _) = return ()
typeCheckItem eT (Init p var e) = do
  let variableName = getNameFromIdent var
  actualType <- typeCheckExpr e
  unless (actualType == eT)
    $ throwPosError p ("Type mismatch variable " ++ show variableName ++ ": " ++ printTree actualType ++ " cannot be assigned to " ++ printTree eT)

-- | Type check arguments.
typeCheckArgs :: BNFC'Position -> [TType] -> [TType] -> TypeChecker ()
typeCheckArgs _ [] [] = return ()
typeCheckArgs p [] _ = throwPosError p "Function application Error: too many arguments"
typeCheckArgs p _ []  = throwPosError p "Function application Error: too few arguments"
typeCheckArgs p (fArg : funArgs) (gArg : givenArgs) = do
  typeCheckArg p fArg gArg
  typeCheckArgs p funArgs givenArgs

typeCheckArg :: BNFC'Position -> TType -> TType -> TypeChecker ()
typeCheckArg pos expected given = do
  let expectedType = getTypeFrom expected
  let givenType = getTypeFrom given
  unless (expectedType == givenType)
  $ throwPosError pos ("Function application Error: expected type " ++ printTree expected ++ ", but got " ++ printTree given ++ " instead.")

-- | Type check types
typeCheckTypes :: [Type] -> TypeChecker ()
typeCheckTypes [] = return ()
typeCheckTypes (t : ts) = do
  _ <- typeCheckType t
  typeCheckTypes ts

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

-- | Type check expressions.
typeCheckExprs :: [Expr] -> TypeChecker ()
typeCheckExprs [] = return ()
typeCheckExprs (e : es) = do
  _ <- typeCheckExpr e
  typeCheckExprs es

typeCheckExpr :: Expr -> TypeChecker TType
typeCheckExpr (ExpVar _ var) = getVarFromEnv var
typeCheckExpr (ExpLitInt _ _) = return $ Integer ()
typeCheckExpr (ExpString _ _) = return $ String ()
typeCheckExpr (ExpLitTrue _) = return $ Boolean ()
typeCheckExpr (ExpLitFalse _) = return $ Boolean ()

typeCheckExpr (ExpApp p f args) = do
  functionArgsTypes <- getFunctionArgTypesFromEnv f
  rtrnType <- getFunctionRetTypeFromEnv f
  unless (length functionArgsTypes == length args)
    $ throwPosError p ("Function application Error: expected " ++ show (length functionArgsTypes) ++ " arguments, but got " ++ show (length args) ++ " instead.")
  givenArgs <- mapM typeCheckExpr args
  typeCheckArgs p functionArgsTypes givenArgs
  return rtrnType

typeCheckExpr (ExpNeg p e) = do
  tempType <- typeCheckExpr e
  let varType = getTypeFrom tempType
  unless (varType == Integer ())
    $ throwPosError p ("Negation Error: expected type was Integer, but got " ++ printTree varType ++ " instead.")
  return $ Integer ()

typeCheckExpr (ExpNot p e) = do
  tempType <- typeCheckExpr e
  let varType = getTypeFrom tempType
  unless (varType == Boolean ())
    $ throwPosError p ("Negation Error: expected type was Boolean, but got " ++ printTree varType ++ " instead.")
  return $ Boolean ()

typeCheckExpr (ExpMul p e1 _ e2) = do
  _ <- typeCheckBinop p e1 e2 [Integer ()]
  return $ Integer ()

typeCheckExpr (ExpAdd p e1 _ e2) = do
  typeCheckBinop p e1 e2 [Integer ()]

typeCheckExpr (ExpRel p e1 _ e2) = do
  _ <- typeCheckBinop p e1 e2 [Integer (), String (), Boolean ()]
  return $ Boolean ()

typeCheckExpr (ExpAnd p e1 e2) = do
  _ <- typeCheckBinop p e1 e2 [Boolean ()]
  return $ Boolean ()

typeCheckExpr (ExpOr p e1 e2) = do
  _ <- typeCheckBinop p e1 e2 [Boolean ()]
  return $ Boolean ()

typeCheckExpr (ExpLambda _ args t blck) = do
  processFunction t args blck
  rtrnT <- typeCheckType t
  let argTypes' = fmap getArgType args
  return $ Function () rtrnT argTypes'

-- | Helper Typechecker functions.
processFunction :: Type -> [Arg] -> FunBlock -> TypeChecker ()
processFunction functionType args blck = do
  env <- get
  forM_ args $ \a -> case a of
    ArgVal _ tp _ -> modify (Map.insert (getArgName a) (omitPosition tp))
    ArgRef _ tp _ -> modify (Map.insert (getArgName a) (omitPositionRef tp))
  let functionType' = omitPosition functionType
  typeCheckFunctionBlock functionType' blck
  put env

typeCheckBinop :: BNFC'Position -> Expr -> Expr -> [TType] -> TypeChecker TType
typeCheckBinop p e1 e2 expected = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  let t1' = getTypeFrom t1
  let t2' = getTypeFrom t2
  unless (t1' `elem` expected && t1' == t2')
    $ throwPosError p ("Binary operation Error: expected types " ++ printTree t1' ++ ", but got " ++ printTree t1' ++ " and " ++ printTree t2' ++ " instead.")
  return t1'
