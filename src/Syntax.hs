{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , TypeOperators 
           , TypeFamilies
           , EmptyCase
           , ScopedTypeVariables
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Syntax where

import Type


--Elem represents an element in a list
data Elem :: [Ty] -> Ty -> * where
  EZ :: Elem (x ': ctx) x
  ES :: Elem xs x -> Elem (y ': xs) x

data ArithOp :: Ty -> * where
  Add :: ArithOp IntTy
  Sub :: ArithOp IntTy
  Eq  :: ArithOp BoolTy
  Lt  :: ArithOp BoolTy
  Gt  :: ArithOp BoolTy
--Expr is a GADT representing well-typed expressions
--expressions with type Expr '[] ty cannot contain free variables, so they are closed
--The restriction on forming well-typed applications is expressed in the AppE constructor, so it is impossible so form an application expr that is not well-typed
data Expr :: [Ty] -> Ty -> * where
  VarE :: Elem ctx ty -> Expr ctx ty
  LamE :: Expr (arg ': ctx) ty -> Expr ctx (arg :-> ty)
  AppE :: Expr ctx (t1 :-> t2) -> Expr ctx t1 -> Expr ctx t2
  IntE :: Int -> Expr ctx IntTy
  LetE :: Expr ctx t1 -> Expr (t1 ': ctx) t2 -> Expr ctx t2
  ArithE :: Expr ctx IntTy -> ArithOp t -> Expr ctx IntTy -> Expr ctx t
  FixE :: Expr ctx (t :-> t) -> Expr ctx t
  CondE :: Expr ctx BoolTy -> Expr ctx t -> Expr ctx t -> Expr ctx t

data Value :: Ty -> * where
  LamV  :: Expr '[arg] ty -> Value (arg :-> ty) 
  IntV  :: Int -> Value IntTy
  BoolV :: Bool -> Value BoolTy
