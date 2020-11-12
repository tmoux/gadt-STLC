{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, TypeOperators, RankNTypes #-}
module Type where

import Data.Type.Equality
--Our types are simply Ints and Functions between types
--Use DataKinds to promote Ty to the type level
--(for instance 'IntTy has kind Ty)
data Ty 
  = IntTy
  | BoolTy
  | Ty :-> Ty
  deriving (Show)
infixr 9 :->

data STy :: Ty -> * where
  SInt  :: STy IntTy
  SBool :: STy BoolTy
  SArr  :: STy t1 -> STy t2 -> STy (t1 :-> t2)

data SCtx :: [Ty] -> * where
  SNil  :: SCtx '[]
  SCons :: STy x -> SCtx xs -> SCtx (x ': xs)

refineTy :: Ty -> (forall t. STy t -> r) -> r
refineTy IntTy k = k SInt
refineTy BoolTy k = k SBool
refineTy (t1 :-> t2) k = refineTy t1 $ \t1' ->
                         refineTy t2 $ \t2' ->
                         k (SArr t1' t2')

equalSTy :: STy t1 -> STy t2 -> Maybe (t1 :~: t2)
equalSTy SInt SInt = Just Refl
equalSTy SBool SBool = Just Refl
equalSTy (SArr s1 s2) (SArr t1 t2) 
  | Just Refl <- equalSTy s1 t1
  , Just Refl <- equalSTy s2 t2 
  = Just Refl
equalSTy _ _ = Nothing

