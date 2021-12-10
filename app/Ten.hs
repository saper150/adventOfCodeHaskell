
module Ten (ten) where

import System.IO
import Control.Monad
import Debug.Trace (traceShow)
import Data.List (sort)


mathingOpeninig ')' = '('
mathingOpeninig ']' = '['
mathingOpeninig '}' = '{'
mathingOpeninig '>' = '<'

bracketScore ')' = 3
bracketScore ']' = 57
bracketScore '}' = 1197
bracketScore '>' = 25137
bracketScore _ = 0


findNotMathing input = _findNotMathing input []
        where _findNotMathing [] state = 'X'   
              _findNotMathing (char:rest) visited
                | char `elem` ['(', '[', '{', '<'] = _findNotMathing rest (char:visited)
                | mathingOpeninig char == head visited = _findNotMathing rest (tail visited)
                | otherwise = char

missingBrackets :: [Char] -> [Char]
missingBrackets input = _missingBrackets input []
        where _missingBrackets [] state = state
              _missingBrackets (char:rest) visited
                | char `elem` ['(', '[', '{', '<'] = _missingBrackets rest (char:visited)
                | mathingOpeninig char == head visited = _missingBrackets rest (tail visited)
                | otherwise = []


calculateScore contents = sum $ map (bracketScore . findNotMathing) (lines contents)


missingBracketScoreMap '(' = 1
missingBracketScoreMap '[' = 2
missingBracketScoreMap '{' = 3
missingBracketScoreMap '<' = 4


missingBracketScore lines =  sorted!! (length sorted `div` 2)
        where mapFn line = foldl (\acc c -> acc * 5 + missingBracketScoreMap c) 0 line
              sorted = filter (>0) $ sort $ map mapFn lines
ten = do
        handle <- openFile "ten-input.txt" ReadMode
        contents <- hGetContents handle
        -- print (calculateScore contents)
        let e = missingBracketScore$ map missingBrackets (lines contents)
        print e
        hClose handle
