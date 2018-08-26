module Main where


-- import Lib
import System.IO
import System.Environment
import Data.Char
import Data.List
import Parser
import Control.Monad

{-
main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile filename
  printout (parse (lines s))
-}

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a

printout :: [String] -> IO ()
printout [] = putStrLn ""
printout (x:xs) = do
  putStrLn x
  printout xs


