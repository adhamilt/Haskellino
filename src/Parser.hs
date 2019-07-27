module Parser where


import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


parse :: String -> [String]
parse x = [x]

languageDef =
   emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "Integer"
                                     ]
           , Token.reservedOpNames = ["+", "-", "=","::", "->"
                                     ]
           }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef


identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer



data Type = Typename String
            deriving (Show)

data ArithmeticBinaryOp = Add
                        | Subtract
                        | Multiply
                        | Divide
                          deriving (Show)

data ArithmeticExpr = Var String
                    | IntConst Integer
                    | Neg ArithmeticExpr
                    | ArithmeticBinary ArithmeticBinaryOp ArithmeticExpr ArithmeticExpr
                      deriving (Show)

data FunctionExpr = FunctionDef String [Type] Type
                  | FunctionBody [String] ArithmeticExpr
                    deriving (Show)

data Statement = Seq [Statement]
          | Assign String ArithmeticExpr
          | Skip
            deriving (Show)

{-
whileParser :: Parser Statement
whileParser = whiteSpace >> statement


statement :: Parser Statement
statement = parens statement
          <|> sequenceOfStatements

-}