{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Pretty where

import Text.Printf ( printf )
import Syntax
import Type

instance Show (STy t) where
  show SInt         = "Int"
  show SBool        = "Bool"
  show (SArr t1 t2) = printf "(%s -> %s)" (show t1) (show t2)

elem_to_int :: Elem ctx ty -> Int
elem_to_int EZ = 0
elem_to_int (ES x) = 1 + elem_to_int x

instance Show (ArithOp t) where
  show Add = "+"
  show Sub = "-"
  show Eq  = "=="
  show Lt  = "<"
  show Gt  = ">"

instance Show (Expr ctx ty) where
  show (VarE elem)       = "#" ++ show (elem_to_int elem)
  show (LamE body)       = printf "(λ#. %s)" (show body)
  show (AppE e1 e2)      = (show e1) ++ " " ++ (show e2)
  show (IntE x)          = show x
  show (LetE e1 e2)      = printf "let # = %s in %s" (show e1) (show e2)
  show (ArithE e1 op e2) = printf "%s %s %s" (show e1) (show op) (show e2)
  show (FixE e)          = printf "fix %s" (show e)
  show (CondE b e1 e2)   = printf "if %s then %s else %s" (show b) (show e1) (show e2)

instance Show (Value ty) where
  show (LamV body) = "λ#." ++ (show body)
  show (IntV x)    = show x
  show (BoolV x)   = show x
