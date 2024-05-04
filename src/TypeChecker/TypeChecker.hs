module TypeChecker.TypeChecker where

-- import ParserLexer.AbsXyzGrammar as AbsXyzGrammar
-- import Data.Map                  as Map
-- -- import qualified Data.Set                  as Set
-- -- import qualified Data.List                 as List
-- -- import qualified Data.Maybe                as Maybe
-- -- import qualified Data.Either               as Either
-- import qualified Control.Monad   as Monad
-- import Control.Monad.State
-- import Control.Monad.Except
-- import Control.Monad.Reader

-- -- | The type of the type checker.
-- type TypeChecker a = ExceptT String (State TypeEnv) a

-- -- | The type environment maps variables to their types.
-- type TypeEnv = Map Ident Type

-- -- | The initial type environment.
-- initialTypeEnv :: TypeEnv
-- initialTypeEnv = Map.empty

-- -- | Run the type checker on a program.
-- runTypeChecker :: Program -> Either String ()
-- runTypeChecker program = runExceptT (checkProgram program) `evalState` initialTypeEnv

-- -- | Check a program.
-- checkProgram :: Program -> TypeChecker ()
-- checkProgram (MyProgram _ stmts) = do
--   Monad.forM_ stmts checkStmt

-- -- | Check a statement.
-- checkStmt :: Stmt -> TypeChecker ()
-- checkStmt stmt = case stmt of
--   Empty _ -> return ()
--   Decl _ ty items -> do
--     Monad.forM_ items $ \item -> case item of
--       NoInit _ _ -> return ()
--       Init _ _ expr -> do
--         ty' <- checkExpr expr
--         if ty == ty'
--           then return ()
--           else throwError "Type mismatch in initialization."
--   Assign _ _ expr -> do
--     ty <- checkExpr expr
--     if ty == ty
--       then return ()
--       else throwError "Type mismatch in assignment."
--   Ret _ expr -> do
--     ty <- checkExpr expr
--     if ty == ty
--       then return ()
--       else throwError "Type mismatch in return statement."
--   VoidRet _ -> return ()
--   If _ cond block -> do
--     ty <- checkExpr cond
--     if ty == Boolean
--       then checkBlock block
--       else throwError "Type mismatch in if condition."
--   IfElse _ cond block1 block2 -> do
--     ty <- checkExpr cond
--     if ty == Boolean
--       then checkBlock block1 >> checkBlock block2
--       else throwError "Type mismatch in if condition."
--   While _ cond block -> do
--     ty <- checkExpr cond
--     if ty == Boolean
--       then checkBlock block
--       else throwError "Type mismatch in while condition."
--   FunctionDef _ _ _ _ _ -> return ()
--   StmtExp _ expr -> do
--     _ <- checkExpr expr
--     return ()

-- -- | Check a block.
-- checkBlock :: Block -> TypeChecker ()
-- checkBlock (StmtBlock _ stmts) = do
--   Monad.forM_ stmts checkStmt

-- -- | Check an expression.
-- checkExpr :: Expr -> TypeChecker Type
-- checkExpr expr = case expr of
--   ExpVar _ ident -> do
--     getType ident
--   ExpLitInt _ _ -> return Integer
--   ExpString _ _ -> return String
--   ExpLitTrue _ -> return Boolean
--   ExpLitFalse _ -> return Boolean
--   ExpApp _ ident exprs -> do
--     ty <- getType ident
--     case ty of
--       Function argTy retTy -> do
--         Monad.forM_ (zip argTy exprs) $ \(ty, expr) -> do
--           ty' <- checkExpr expr
--           if ty == ty'
--             then return ()
--             else throwError "Type mismatch in function application."
--         return retTy
--       _ -> throwError "Function expected."
--   ExpNeg _ expr -> do
--     ty <- checkExpr expr
--     if ty == Integer
--       then return Integer
--       else throwError "Type mismatch in negation."
--   ExpNot _ expr -> do
--     ty <- checkExpr expr
--     if ty == Boolean
--       then return Boolean
--       else throwError "Type mismatch in negation."
--   ExpMul _ expr1 expr2 -> checkBinOp expr1 expr2
--   ExpAdd _ expr1 expr2 -> checkBinOp expr1 expr2
--   ExpRel _ expr1 expr2 -> checkBinOp expr1 expr2
--   ExpAnd _ expr1 expr2 -> checkBinOp expr1 expr2
--   ExpOr _ expr1 expr2 -> checkBinOp expr1 expr2
--   ExpLambda _ args ty expr -> do
--     Monad.forM_ args $ \arg -> case arg of
--       ArgVal _ _ _ -> return ()
--       ArgRef _ _ _ -> return ()
--     ty' <- checkExpr expr
--     if ty == ty'
--       then return (Function (Prelude.map argType args) ty)
--       else throwError "Type mismatch in lambda."

-- -- | Check a binary operation.
-- checkBinOp :: Expr -> Expr -> TypeChecker Type
-- checkBinOp expr1 expr2 = do
--   ty1 <- checkExpr expr1
--   ty2 <- checkExpr expr2
--   if ty1 == ty2
--     then return ty1
--     else throwError "Type mismatch in binary operation."

-- -- | Get the type of a variable.
-- getType :: Ident -> TypeChecker Type
-- getType ident = do
--   env <- get
--   case Map.lookup ident env of
--     Just ty -> return ty
--     Nothing -> throwError "Variable not in scope."

-- -- | Add a variable to the type environment.
-- addVar :: Ident -> Type -> TypeChecker ()
-- addVar ident ty = do
--   env <- get
--   put (Map.insert ident ty env)

-- -- | Add a function to the type environment.
-- addFun :: Ident -> [Type] -> Type -> TypeChecker ()
-- addFun ident argTy retTy = do
--   env <- get
--   put (Map.insert ident (Function argTy retTy) env)


