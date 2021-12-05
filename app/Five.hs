
module Five (five) where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (traceShow)


sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

lineFromTo::(Int, Int)->(Int, Int) -> [(Int, Int)]
lineFromTo (fx,fy) (tx, ty) = followDirection (fx,fy) (tx, ty)
        where (dirX, dirY) = (sign (tx - fx), sign (ty - fy))
              followDirection (cx, cy) dest
                | cx == fst dest && cy == snd dest = [dest]
                | otherwise = (cx, cy) : followDirection (cx + dirX, cy + dirY) dest


parseLine :: [Char] -> ((Int, Int), (Int, Int))
parseLine line = (parsePart from, parsePart to)
    where [from, to] = splitOn " -> " line
          parsePart p = ((read::String->Int) x, (read::String->Int) y)
            where [x, y] = splitOn "," p


parse input = map parseLine (lines input)

applyMove board (from, to) = foldl foldFn board line
    where line = lineFromTo from to
          foldFn board point = Map.alter alterFn point board
          alterFn (Just x) = Just (x + 1)
          alterFn Nothing = Just 1

applyMoves::[((Int,Int), (Int,Int))] -> Map.Map (Int,Int) Int
applyMoves = foldl applyMove Map.empty

countOverlaping::Map.Map (Int,Int) Int -> Int
countOverlaping = Map.foldl foldFn 0
    where foldFn acc 1 = acc
          foldFn acc x = acc + 1
five = do
        handle <- openFile "five-input.txt" ReadMode
        contents <- hGetContents handle
        let b = applyMoves (parse contents)
        print (countOverlaping b)
        hClose handle
