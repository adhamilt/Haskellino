module Parser where


import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


parser :: String -> Either ParseError [Expression]
parser input = parse expression "(unknown)" input


expression :: Parser [Expression]
expression = sepBy1 statement (eol)

statement :: Parser Expression
statement = Expr.buildExpressionParser table factor

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"


languageDef =
  emptyDef { Token.commentStart    = "/*",
             Token.commentEnd      = "*/",
             Token.commentLine     = "//",
             Token.identStart      = letter,
             Token.identLetter     = alphaNum,
             Token.reservedNames   = [ "true",
                                       "false",
                                       "and",
                                       "or",
                                       "not"
                                     ],
             Token.reservedOpNames = [ "+", "-", "*", "/",
                                       "<", ">", "and", "or", "not"
                                     ],
             Token.caseSensitive   = True
  }


lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

infixOp :: String -> (a -> a) -> Expr.Operator String () Identity a
infixOp s f = Expr.Infix (reservedOp s >> return f)


table :: Ex.OperatorTable String () Identity Expr
table = [
        [
          infixOp "+" Add,
          infixOp "-" Subtract,
          infixOp "*" Multiply,
          infixOp "/" Divide
        ]
        ]

true, false :: Parser statement
true = reserved "true" (BoolConst True)
false = reserved "false" (BoolConst False)

integer :: Parser statement
integer = Token.integer lexer 

factor :: Parser statement
factor = true
       <|> false
       <|> parens statement

data Type = Typename String
            deriving (Show)

data ArithmeticBinaryOp = Add
                        | Subtract
                        | Multiply
                        | Divide
                          deriving (Show)

data ArithmeticExpr = Var String
                    | IntConst Integer
                    | BoolConst Bool
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