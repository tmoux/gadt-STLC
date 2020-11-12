{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , TypeOperators
           , TypeFamilies
           , EmptyCase
           , ScopedTypeVariables
           , RankNTypes
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Typecheck where

import Control.Monad.Except
import Type
import Data.Type.Equality
import UncheckedSyntax
import Syntax
import Text.Printf

type TCMonad a = Except String a

checkOp :: OpU -> (forall t. STy t -> ArithOp t -> TCMonad a) -> TCMonad a
checkOp AddU k = k SInt Add
checkOp SubU k = k SInt Sub
checkOp EqU  k = k SBool Eq
checkOp LtU  k = k SBool Lt
checkOp GtU  k = k SBool Gt

check :: UExpr -> (forall t. STy t -> Expr '[] t -> TCMonad a) -> TCMonad a
check = check' SNil where
  check' :: SCtx ctx -> UExpr -> (forall t. STy t -> Expr ctx t -> TCMonad a) -> TCMonad a
  check' ctx (VarU i) k = check_index ctx i $ \ty el -> k ty (VarE el)
  check' ctx (LamU ty body) k =
    refineTy ty $ \sty ->
      check' (SCons sty ctx) body $ \bodyTy body' ->
        k (SArr sty bodyTy) (LamE body')
  check' ctx (AppU e1 e2) k = check' ctx e1 $ \ty1 e1' ->
                            check' ctx e2 $ \ty2 e2' ->
                            case (ty1,ty2) of
                              (SArr arg_ty ret_ty,arg_ty') | Just Refl <- equalSTy arg_ty arg_ty' ->
                                k ret_ty (AppE e1' e2')
                              _ -> throwError (printf "invalid application: function has type %s, argument has type %s" (show ty1) (show ty2))
  check' ctx (IntU x) k = k SInt (IntE x)
  check' ctx (LetU e1 e2) k = check' ctx e1 $ \ty1 e1' ->
                              check' (SCons ty1 ctx) e2 $ \ty2 e2' ->
                              k ty2 (LetE e1' e2')
  check' ctx (ArithU e1 op e2) k = check' ctx e1 $ \ty1 e1' ->
                                   check' ctx e2 $ \ty2 e2' ->
                                   checkOp op $ \opTy op' ->
                                   case (ty1,ty2) of
                                     (SInt,SInt) -> k opTy (ArithE e1' op' e2')
                                     _ -> throwError (printf "invalid add expr: operands have type %s and %s, the operation is %s" (show ty1) (show ty2) (show op'))
  check' ctx (FixU e) k = check' ctx e $ \ty e' ->
                          case ty of
                            SArr t1 t2 | Just Refl <- equalSTy t1 t2 ->
                              k t1 (FixE e')
                            _ -> throwError (printf "invalid fix: function has type %s, expecting a function of type a -> a" (show ty))
  check' ctx (CondU b e1 e2) k =
    check' ctx b $ \tyb b' ->
    check' ctx e1 $ \ty1 e1' ->
    check' ctx e2 $ \ty2 e2' ->
    case tyb of
      _ | Just Refl <- equalSTy tyb SBool
        , Just Refl <- equalSTy ty1 ty2 ->
        k ty1 (CondE b' e1' e2')
      _ -> throwError (printf "invalid if expr: conditional has type %s, two branches have type %s and %s" (show tyb) (show ty1) (show ty2))

  check_index :: SCtx ctx -> Int -> (forall t. STy t -> Elem ctx t -> TCMonad a) -> TCMonad a
  check_index SNil _ k = throwError "unbound variable, parser should've caught this"
  check_index (SCons t _)  0 k = k t EZ
  check_index (SCons _ ts) n k = check_index ts (n-1) $ \t el -> k t (ES el)
