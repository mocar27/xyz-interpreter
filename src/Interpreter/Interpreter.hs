module Interpreter.Interpreter ( run, runFile ) where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, print
  )
import ParserLexer.AbsXyzGrammar   ()
import ParserLexer.LexXyzGrammar   ( Token, mkPosToken )
import ParserLexer.ParXyzGrammar   ( pProgram, myLexer )
import ParserLexer.PrintXyzGrammar ( Print, printTree )
import System.Exit                 ( exitFailure )

-- import TypeChecker.TypeChecker     ( runTypeChecker )

type ParseFun a = [Token] -> Either String a

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      mapM_ (putStrLn . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      print tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

-- parseAndPrintFile :: FilePath -> IO ()
-- parseAndPrintFile filePath = do
--   input <- readFile filePath
--   parseAndPrintAST input

-- parseAndPrintAST :: String -> IO ()
-- parseAndPrintAST input = do
--   case parseInput input of
--     Left err -> putStrLn $ "Error: " ++ err
--     Right tree -> putStrLn $ "Parsed AST: " ++ show tree
