module Lexemes where


--Whitespace

data WhiteStuff = WhiteChar WChar | Comment | NComment

data WChar = NLine Newline | VerTab | Space | Tab | UniWhite

data Newline = Return | LineFeed | FormFeed

isWhiteSpace :: Char -> Bool
isWhiteSpace = isWhiteStuff


isWhiteStuff :: Char -> Bool
isWhiteStuff x
  | isWhiteChar x = True
  | otherwise = False


isWhiteChar :: Char -> Bool
isWhiteChar x
  | isNewline x = True
  | isSpace x = True
  | x=='\t' = True
  | x=='\v' = True
  | otherwise = False


isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False


isNewline :: Char -> Bool
isNewline '\n' = True
isNewline '\r' = True
isNewline '\f' = True
isNewline _ = False