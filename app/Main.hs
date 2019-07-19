module Main where


-- import Lib
import System.IO
import System.Environment
import Data.Char
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile filename
  printout (myparse (lines s))


myparse :: [String] -> [String]
myparse x = x

printout :: [String] -> IO ()
printout [] = return ()
printout (x:xs) = do
  print x
  printout xs
