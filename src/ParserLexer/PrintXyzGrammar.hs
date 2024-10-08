-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintXyzGrammar.

module ParserLexer.PrintXyzGrammar where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified ParserLexer.AbsXyzGrammar as AbsXyzGrammar

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsXyzGrammar.Ident where
  prt _ (AbsXyzGrammar.Ident i) = doc $ showString i
instance Print (AbsXyzGrammar.Program' a) where
  prt i = \case
    AbsXyzGrammar.MyProgram _ stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print [AbsXyzGrammar.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsXyzGrammar.Arg' a) where
  prt i = \case
    AbsXyzGrammar.ArgVal _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
    AbsXyzGrammar.ArgRef _ type_ id_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 type_, prt 0 id_])

instance Print [AbsXyzGrammar.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsXyzGrammar.Item' a) where
  prt i = \case
    AbsXyzGrammar.NoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsXyzGrammar.Init _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsXyzGrammar.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsXyzGrammar.Block' a) where
  prt i = \case
    AbsXyzGrammar.StmtBlock _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print (AbsXyzGrammar.FunBlock' a) where
  prt i = \case
    AbsXyzGrammar.FnBlock _ stmts rtrn -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, prt 0 rtrn, doc (showString "}")])

instance Print (AbsXyzGrammar.Stmt' a) where
  prt i = \case
    AbsXyzGrammar.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsXyzGrammar.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsXyzGrammar.Assign _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsXyzGrammar.If _ expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsXyzGrammar.IfElse _ expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsXyzGrammar.While _ expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsXyzGrammar.FunctionDef _ type_ id_ args funblock -> prPrec i 0 (concatD [prt 0 type_, doc (showString "function"), prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 funblock])
    AbsXyzGrammar.StmtExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])

instance Print (AbsXyzGrammar.Rtrn' a) where
  prt i = \case
    AbsXyzGrammar.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])

instance Print [AbsXyzGrammar.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsXyzGrammar.Type' a) where
  prt i = \case
    AbsXyzGrammar.Integer _ -> prPrec i 0 (concatD [doc (showString "Integer")])
    AbsXyzGrammar.String _ -> prPrec i 0 (concatD [doc (showString "String")])
    AbsXyzGrammar.Boolean _ -> prPrec i 0 (concatD [doc (showString "Boolean")])
    AbsXyzGrammar.RefInteger _ -> prPrec i 0 (concatD [doc (showString "RefInteger")])
    AbsXyzGrammar.RefString _ -> prPrec i 0 (concatD [doc (showString "RefString")])
    AbsXyzGrammar.RefBoolean _ -> prPrec i 0 (concatD [doc (showString "RefBoolean")])
    AbsXyzGrammar.Function _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])

instance Print [AbsXyzGrammar.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsXyzGrammar.Expr' a) where
  prt i = \case
    AbsXyzGrammar.ExpVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsXyzGrammar.ExpLitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsXyzGrammar.ExpString _ str -> prPrec i 6 (concatD [printString str])
    AbsXyzGrammar.ExpLitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsXyzGrammar.ExpLitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsXyzGrammar.ExpApp _ id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsXyzGrammar.ExpNeg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsXyzGrammar.ExpNot _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsXyzGrammar.ExpMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsXyzGrammar.ExpAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsXyzGrammar.ExpRel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsXyzGrammar.ExpAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsXyzGrammar.ExpOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    AbsXyzGrammar.ExpLambda _ args type_ funblock -> prPrec i 0 (concatD [doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "=>"), prt 0 type_, prt 0 funblock])

instance Print (AbsXyzGrammar.AddOp' a) where
  prt i = \case
    AbsXyzGrammar.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsXyzGrammar.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsXyzGrammar.MulOp' a) where
  prt i = \case
    AbsXyzGrammar.Multi _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsXyzGrammar.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsXyzGrammar.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsXyzGrammar.RelOp' a) where
  prt i = \case
    AbsXyzGrammar.LThan _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsXyzGrammar.Leq _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsXyzGrammar.GThan _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsXyzGrammar.Geq _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsXyzGrammar.Eq _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsXyzGrammar.NEq _ -> prPrec i 0 (concatD [doc (showString "!=")])
