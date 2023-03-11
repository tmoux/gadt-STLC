module Parser where

import           Control.Monad.Reader
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec          hiding (Parser)
import           Text.ParserCombinators.Parsec.Language

import           Lexer
import           Type
import           UncheckedSyntax

--the parser creates a UExpr (unchecked expression) with de Brujin indices
type Parser = ParsecT String () (Reader [String])

parseExpr :: String -> Either ParseError UExpr
parseExpr input = runReader (runParserT expr () "" input) []

parseTy :: Parser Ty
parseTy = baseTy `chainr1` (resOp "->" >> return (:->))

baseTy :: Parser Ty
baseTy =
  choice [IntTy <$ reserved "Int", BoolTy <$ reserved "Bool", parens parseTy]

expr :: Parser UExpr
expr = choice [lambda, bool_exp, cond_exp, let_exp]

lambda :: Parser UExpr
lambda = do
  reserved "lam"
  x <- identifier
  resOp ":"
  ty <- parseTy
  resOp "."
  e <- local (x :) expr
  return $ LamU ty e

arith_ops =
  choice
    [ (\x y -> ArithU x AddU y) <$ resOp "+"
    , (\x y -> ArithU x SubU y) <$ resOp "-"
    ]

bool_ops =
  choice
    [ (\x y -> ArithU x EqU y) <$ resOp "=="
    , (\x y -> ArithU x LtU y) <$ resOp "<"
    , (\x y -> ArithU x GtU y) <$ resOp ">"
    ]

bool_exp :: Parser UExpr
bool_exp = arith_exp `chainl1` bool_ops

arith_exp :: Parser UExpr
arith_exp = term `chainl1` arith_ops

cond_exp :: Parser UExpr
cond_exp = do
  reserved "if"
  b <- expr
  reserved "then"
  e1 <- expr
  reserved "else"
  e2 <- expr
  return $ CondU b e1 e2

let_exp :: Parser UExpr
let_exp = do
  reserved "let"
  x <- identifier
  resOp "="
  e1 <- expr
  reserved "in"
  e2 <- local (x :) expr
  return $ LetU e1 e2

term :: Parser UExpr
term = choice [primary `chainl1` return AppU, FixU <$ reserved "fix" <*> expr]

primary :: Parser UExpr
primary = choice [parens expr, var, (IntU . fromIntegral) <$> integer]

var :: Parser UExpr
var = do
  v <- identifier
  idx <- reader $ getIndex v
  case idx of
    Just x  -> return $ VarU x
    Nothing -> fail ("unbound variable " ++ v)

--get de Brujin index or Nothing
getIndex :: String -> [String] -> Maybe Int
getIndex v [] = Nothing
getIndex v (x:xs)
  | x == v = Just 0
  | otherwise = (1 +) <$> getIndex v xs
