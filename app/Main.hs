module Main where


-- import Lib
import System.IO
import System.Environment
import Data.Char
import Data.List
import Parser
import Text.ParserCombinators.Parsec.Error

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    printout (parser s)


printout :: Either ParseError [[String]] -> IO ()
printout s = case s of
    Left e  -> putStrLn "Error parsing input: " >> print e --Print out an error message
    Right r -> mapM_ print r --print out the tokens


