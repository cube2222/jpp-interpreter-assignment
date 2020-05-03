{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.Print where

-- pretty-printer generated by the BNF converter

import Language.Abs
import Data.Char


-- the top-level printing method
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
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
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
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print (TypeName a) where
  prt i e = case e of
    TSimpleTypeName _ id -> prPrec i 0 (concatD [prt 0 id])
    TPolymorphicTypeName _ id typenames -> prPrec i 0 (concatD [prt 0 id, doc (showString "<"), prt 0 typenames, doc (showString ">")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (FunctionParameter a) where
  prt i e = case e of
    AFunctionArgument _ id typename -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 typename])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Stmt a) where
  prt i e = case e of
    SDeclVar _ id expr -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString "="), prt 0 expr])
    SDeclFun _ id functionparameters typename expr -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id, doc (showString "("), prt 0 functionparameters, doc (showString ")"), doc (showString ":"), prt 0 typename, doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print (MatchClause a) where
  prt i e = case e of
    MMatchClause _ expr1 expr2 -> prPrec i 0 (concatD [doc (showString "as"), prt 1 expr1, doc (showString "~>"), prt 2 expr2])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Expr a) where
  prt i e = case e of
    EAdd _ expr1 expr2 -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "+"), prt 7 expr2])
    ESub _ expr1 expr2 -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "-"), prt 7 expr2])
    EMul _ expr1 expr2 -> prPrec i 7 (concatD [prt 7 expr1, doc (showString "*"), prt 8 expr2])
    EDiv _ expr1 expr2 -> prPrec i 7 (concatD [prt 7 expr1, doc (showString "/"), prt 8 expr2])
    EEq _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "=="), prt 6 expr2])
    ENotEq _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "!="), prt 6 expr2])
    ELt _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "<"), prt 6 expr2])
    EGt _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString ">"), prt 6 expr2])
    ELtEq _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "<="), prt 6 expr2])
    EGtEq _ expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString ">="), prt 6 expr2])
    EInt _ n -> prPrec i 10 (concatD [prt 0 n])
    ENegInt _ n -> prPrec i 10 (concatD [doc (showString "-"), prt 0 n])
    EOr _ expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "or"), prt 5 expr2])
    EAnd _ expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "and"), prt 5 expr2])
    ENot _ expr -> prPrec i 7 (concatD [doc (showString "not"), prt 8 expr])
    ETrue _ -> prPrec i 10 (concatD [doc (showString "true")])
    EFalse _ -> prPrec i 10 (concatD [doc (showString "false")])
    ELambda _ functionparameter expr -> prPrec i 9 (concatD [doc (showString "("), prt 0 functionparameter, doc (showString "->"), prt 0 expr, doc (showString ")")])
    EVar _ id -> prPrec i 10 (concatD [prt 0 id])
    EFunCall _ expr exprs -> prPrec i 9 (concatD [prt 9 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    ECons _ expr1 expr2 -> prPrec i 4 (concatD [prt 5 expr1, doc (showString "::"), prt 4 expr2])
    EList _ exprs -> prPrec i 5 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    ENil _ -> prPrec i 5 (concatD [doc (showString "nil")])
    EIfte _ expr1 expr2 expr3 -> prPrec i 3 (concatD [doc (showString "if"), prt 3 expr1, doc (showString "then"), prt 3 expr2, doc (showString "else"), prt 3 expr3])
    ESemicolon _ stmt expr -> prPrec i 1 (concatD [prt 0 stmt, doc (showString ";"), prt 1 expr])
    EMatch _ expr matchclauses -> prPrec i 0 (concatD [doc (showString "match"), prt 1 expr, prt 0 matchclauses])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

