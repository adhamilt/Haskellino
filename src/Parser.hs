module Parser where


import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


parser :: String -> Either ParseError [[String]]
parser input = parse expression "(unknown)" input

expression :: Parser [[String]]
expression = endBy1 statement eof

statement :: Parser [String]
statement = sepBy1 line (eol)

line :: Parser String
line =  many1 (noneOf "\n\r")

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"


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

data Expression = Seq [Expression]
          | Assign String ArithmeticExpr
          | Skip
            deriving (Show)