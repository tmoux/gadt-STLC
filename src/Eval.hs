{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , TypeOperators 
           , TypeFamilies
           , EmptyCase
           , ScopedTypeVariables
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Eval where

import Type
import Syntax

data Fin :: [Ty] -> * where
  FZ :: Fin '[]
  FS :: Fin xs -> Fin (x ': xs)

type family (as :: [Ty]) ++ (bs :: [Ty]) :: [Ty]
type instance '[]       ++ bs = bs
type instance (a ': as) ++ bs = a ': (as ++ bs)
infixr 5 ++

eval :: Expr '[] ty -> Value ty
eval e = case step e of
  Left v   -> v
  Right e' -> eval e'

--step shows that small-step evaluation preserves types (preservation)
--every closed expression evaluates to a value or another expression (progress)
evalArith :: Int -> ArithOp t -> Int -> Value t
evalArith x Add y = IntV $ x + y
evalArith x Sub y = IntV $ x - y
evalArith x Eq  y = BoolV $ x == y
evalArith x Lt  y = BoolV $ x < y
evalArith x Gt  y = BoolV $ x > y

step :: Expr '[] ty -> Either (Value ty) (Expr '[] ty)
step (LamE body)  = Left $ LamV body
step (IntE x)     = Left $ IntV x
step (VarE elem)  = case elem of {}
step (AppE e1 e2) = case step e1 of
  Left (LamV body) -> Right $ subst e2 body
  Right e1'        -> Right $ AppE e1' e2
step (LetE e1 e2) = Right $ subst e1 e2 
step (ArithE x op y)   = case (step x,step y) of
  (Left (IntV xx), Left (IntV yy)) -> Left  $ evalArith xx op yy
  (Left _, Right y')               -> Right $ ArithE x op y'
  (Right x', _)                    -> Right $ ArithE x' op y
step (FixE e)     = case step e of 
  Left (LamV body) -> Right $ subst (FixE (LamE body)) body
  Right e'         -> Right $ FixE e'
step (CondE b e1 e2) = case step b of
  Left (BoolV True)  -> step e1
  Left (BoolV False) -> step e2
  Right b'           -> Right $ CondE b' e1 e2

shift :: forall ctx x ty. Expr ctx ty -> Expr (x ': ctx) ty
shift = shift' FZ
  where
  shift' :: forall fs ty. Fin fs -> Expr (fs ++ ctx) ty -> Expr (fs ++ x ': ctx) ty
  shift' fn (VarE y)          = VarE (shift_var fn y)
  shift' fn (LamE body)       = LamE (shift' (FS fn) body)
  shift' fn (AppE e1 e2)      = AppE (shift' fn e1) (shift' fn e2)
  shift' fn (IntE x)          = IntE x
  shift' fn (LetE e1 e2)      = LetE (shift' fn e1) (shift' (FS fn) e2)
  shift' fn (ArithE e1 op e2) = ArithE (shift' fn e1) op (shift' fn e2)
  shift' fn (FixE e)          = FixE (shift' fn e)
  shift' fn (CondE b e1 e2)   = CondE (shift' fn b) (shift' fn e1) (shift' fn e2)

  shift_var :: Fin fs -> Elem (fs ++ ctx) t -> Elem (fs ++ x ': ctx) t
  shift_var FZ     e      = ES e
  shift_var (FS x) EZ     = EZ
  shift_var (FS x) (ES y) = ES (shift_var x y)

subst :: forall ctx arg ty. Expr ctx arg -> Expr (arg ': ctx) ty -> Expr ctx ty
subst e = subst' e FZ
  where 
  subst' :: forall fs ctx t. Expr ctx arg -> Fin fs -> Expr (fs ++ arg ': ctx) t -> Expr (fs ++ ctx) t
  subst' e FZ     (VarE EZ)          = e
  subst' e FZ     (VarE (ES y))      = VarE y
  subst' e (FS x) (VarE EZ)          = VarE EZ
  subst' e (FS x) (VarE (ES y))      = shift (subst' e x (VarE y))
  subst' e fin    (LamE body)        = LamE (subst' e (FS fin) body)
  subst' e fin    (AppE e1 e2)       = AppE (subst' e fin e1) (subst' e fin e2)
  subst' e fin    (IntE x)           = IntE x
  subst' e fin    (LetE e1 e2)       = LetE (subst' e fin e1) (subst' e (FS fin) e2)
  subst' e fin    (ArithE e1 op e2)  = ArithE (subst' e fin e1) op (subst' e fin e2)
  subst' e fin    (FixE e')          = FixE (subst' e fin e')  
  subst' e fin    (CondE b e1 e2)    = CondE (subst' e fin b) (subst' e fin e1) (subst' e fin e2)
