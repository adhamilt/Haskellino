module Parser where

import Data.Char
import Data.List
import Data.Maybe


parse :: [String]->[String]
parse x = stripComments x


stripComments :: [String] -> [String]
--stripComments x = catMaybes (map uncomment x)
stripComments [] = []
stripComments x = lines (scanComments (unlines x))

scanComments :: String -> String
scanComments [] = []
scanComments [x] = [x]
scanComments "--" = []
scanComments "-{" = undefined
scanComments "-}" = undefined
scanComments [x,y] = [x,y] -- two characters could be a comment
scanComments x
  | a == '-' && b == '-' && c /= '>' = inComments ([c]++xs)
  | a == '|' && b == '-' && c == '-' = [a,b,c] ++ scanComments xs
  | a == '{' && b == '-' = inMultiComments ([c] ++ xs) False
  | otherwise = [a] ++ (scanComments ([b,c] ++ xs))
  where
  a = head x
  b = head (tail x)
  c = head (tail (tail x))
  xs = tail (tail (tail x))

inComments :: String -> String
inComments [] = []  --This should produce an error
inComments (x:xs)
  | x =='\n' = scanComments xs
  | otherwise = inComments xs

inMultiComments :: String -> Bool -> String
inMultiComments [] _ = undefined  --This should produce an error
inMultiComments [x] _ = undefined --This is also an error
inMultiComments (a:b:xs) isMultiLine
  | a == '-' && b == '}' && isMultiLine = scanComments (['\n'] ++ xs)
  | a == '-' && b == '}' = scanComments (xs)
  | a == '\n' = inMultiComments ([b] ++ xs) True
  | otherwise = inMultiComments ([b] ++ xs) isMultiLine


uncomment :: String -> Maybe String
uncomment x = case (isComment x) of
  True -> Nothing
  False -> Just x

isComment :: String -> Bool
isComment x
  | a == '-' && b == '-' = True
  | isSpace(a) = isComment ([b]++xs)
  | otherwise = False
  where
  a = head x
  b = head (tail x)
  xs = tail (tail x)

removeComments :: ([String],Bool) -> ([String],Bool)
removeComments ((line:lines),inBlock)
  | a == '-' && b == ' ' = ([],False)
  | otherwise = ([],False)
    where
    a = head line
    b = head (tail line)
    xs = tail (tail line)