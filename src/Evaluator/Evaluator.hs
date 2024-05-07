module Evaluator.Evaluator where

-- import Prelude ( ($), IO, putStrLn, Either(..), String, Integer, Bool, show, (>>), (>>=), undefined )
-- import qualified Prelude as C ( Show )
-- import qualified Data.Map as Map ( Map, empty, insert, lookup )
-- import qualified Data.Maybe as Maybe ( fromJust, isJust )
-- import qualified ParserLexer.AbsXyzGrammar as Abs ( Program(..), Stmt(..), Expr(..), Type(..), Item(..), Block(..), Arg(..), Program' (MyProgram) )

-- -- | The environment is a map from variable names to values.
-- type Env = Map.Map String Value

-- -- | The value type.
-- data Value
--     = VInt Integer
--     | VString String
--     | VBool Bool
--     | VVoid
--     | VFunction [String] Abs.Block
--     | VRef String
--     deriving (C.Show)

-- -- | Evaluate a program.
-- -- evalProgram :: Abs.Program -> IO ()
-- -- evalProgram (Abs.MyProgram _ stmts) = evalStmts stmts Map.empty

-- runEvaluator :: Abs.Program -> IO ()
-- runEvaluator = undefined