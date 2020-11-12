module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Control.Monad.Reader
import qualified Text.ParserCombinators.Parsec.Token as Token

langDef :: GenLanguageDef String () (Reader [String])
langDef = 
    LanguageDef { Token.commentStart    = "/*"
                , Token.commentEnd      = "*/"
                , Token.commentLine     = "//"
                , Token.nestedComments  = True
                , Token.identStart      = letter <|> char '_'
                , Token.identLetter     = alphaNum <|> char '_'
                , Token.opStart         = oneOf ":.+-<>="
                , Token.opLetter        = oneOf ":.+-<>="
                , Token.reservedNames   = [ "lam"
                                          , "let"
                                          , "in"
                                          , "fix"
                                          , "if"
                                          , "then"
                                          , "else"
                                          ]
                , Token.reservedOpNames = [ ":" 
                                          , "."
                                          , "+"
                                          , "-"
                                          , "->"
                                          , "="
                                          , "=="
                                          , "<"
                                          , ">"
                                          ]
                , Token.caseSensitive   = True
    }
              
lexer = Token.makeTokenParser langDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
resOp      = Token.reservedOp lexer
parens     = Token.parens lexer
integer    = Token.natural lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer

