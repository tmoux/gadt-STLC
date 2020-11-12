module UncheckedSyntax where

import Type
import Pretty
import Syntax

data OpU 
  = AddU
  | SubU
  | EqU
  | LtU
  | GtU
  deriving (Show)

data UExpr
  = VarU Int
  | LamU Ty UExpr
  | AppU UExpr UExpr
  | IntU Int
  | LetU UExpr UExpr
  | ArithU UExpr OpU UExpr
  | FixU UExpr
  | CondU UExpr UExpr UExpr
  deriving (Show)
