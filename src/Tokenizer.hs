module Tokenizer where

import Data.Semigroup
import Lexemes



data Token = Const String | WhiteSpace WhiteStuff | Empty



instance Show Newline where
  show Return = "Return"
  show LineFeed = "LineFeed"
  show FormFeed = "FormFeed"

instance Show WChar where
  show (NLine x) = (show x)
  show VerTab = "Vertab"
  show Space = "Space"
  show Tab = "Tab"
  show UniWhite = "UniWhite"

instance Show WhiteStuff where
  show (WhiteChar x) = show x
  show Comment = "Comment"
  show NComment = "NComment"

instance Show Token where
  show (Const x) = show x
  show (WhiteSpace x) = show x
  show Empty = "Empty"

{-
instance Semigroup Token where
  (<>) (Const a) (Const b) = Const (a++b)
  (<>) (WhiteSpace a) (WhiteSpace b) = WhiteSpace (a++b)
  (<>) x Empty = x


instance Monoid Token where
  mempty = Empty
  mappend = (<>)
-}

type Tokenizer = ([Token],String)


tokenize :: Tokenizer -> Tokenizer
tokenize (t,[]) = (t,[])
tokenize (t,(x:xs))
 |isWhiteSpace x = whitespace (t,(x:xs))
 |otherwise = tokenize (t++[Const [x]], xs)


whitespace :: Tokenizer -> Tokenizer
whitespace (t,[]) = (t,[])
whitespace (t,x:xs)
  | x==' '  = tokenize   (t++[WhiteSpace (WhiteChar Space)],xs)
  | x=='\n' = tokenize   (t++[WhiteSpace (WhiteChar (NLine LineFeed))],xs)
  | x=='\r' = tokenize   (t++[WhiteSpace (WhiteChar (NLine Return))],xs)
  | x=='\f'  = tokenize  (t++[WhiteSpace (WhiteChar (NLine FormFeed))],xs)
  | x=='\v'  = tokenize  (t++[WhiteSpace (WhiteChar VerTab)],xs)
  | x=='\t'  = tokenize  (t++[WhiteSpace (WhiteChar Tab)],xs)
  | otherwise = tokenize (t++[Empty], xs)


runTokenizer :: String -> [Token]
runTokenizer [] = []
runTokenizer inp = x
  where (x,_) = tokenize ([],inp)


