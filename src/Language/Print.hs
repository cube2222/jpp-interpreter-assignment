{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Language.
--   Generated by the BNF converter.

module Language.Print where

import qualified Language.Abs
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

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
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Language.Abs.Ident where
  prt _ (Language.Abs.Ident i) = doc (showString i)

instance Print Language.Abs.Expr where
  prt i e = case e of
    Language.Abs.EAdd expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "+"), prt 1 expr2])
    Language.Abs.ESub expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "-"), prt 1 expr2])
    Language.Abs.EMul expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "*"), prt 2 expr2])
    Language.Abs.EDiv expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "//"), prt 2 expr2])
    Language.Abs.EInt n -> prPrec i 2 (concatD [prt 0 n])
    Language.Abs.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "or"), prt 1 expr2])
    Language.Abs.EAnd expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "and"), prt 1 expr2])
    Language.Abs.ENot expr -> prPrec i 1 (concatD [doc (showString "not"), prt 2 expr])
    Language.Abs.ETrue -> prPrec i 2 (concatD [doc (showString "true")])
    Language.Abs.EFalse -> prPrec i 2 (concatD [doc (showString "false")])
    Language.Abs.EVar id -> prPrec i 2 (concatD [prt 0 id])
    Language.Abs.EFunCall id expr -> prPrec i 2 (concatD [prt 0 id, doc (showString "("), prt 0 expr, doc (showString ")")])
    Language.Abs.ESemicolon stmt expr -> prPrec i 0 (concatD [prt 0 stmt, doc (showString ";"), prt 0 expr])

instance Print Language.Abs.Stmt where
  prt i e = case e of
    Language.Abs.SDeclVar id expr -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString "="), prt 0 expr])
    Language.Abs.SDeclFun id1 id2 expr -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id1, doc (showString "("), prt 0 id2, doc (showString ")"), doc (showString "{"), prt 0 expr, doc (showString "}")])

