module Parser where

import Data.Char
import Data.List
import Data.Maybe


parse :: [String]->[String]
parse x = stripComments x


stripComments :: [String] -> [String]
stripComments x = catMaybes (map uncomment x)

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
