module Main where


-- import Lib
import System.IO
import System.Environment
import Data.Char
import Data.List
import Parser


main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile filename
  printout (parse s)


printout :: [String] -> IO ()
printout [] = putStrLn ""
printout (x:xs) = do
  putStrLn x
  printout xs


