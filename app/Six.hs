
module Six (six) where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import qualified Data.Map as Map

emptyMap::Map.Map Int Int
emptyMap = Map.fromList [(x, 0) | x <- [0..8]]

advanceDay::Map.Map Int Int -> Map.Map Int Int
advanceDay map = case Map.lookup (-1) m of
        Just x -> (Map.delete (-1) . Map.insertWith (+) 8 x . Map.insertWith (+) 6 x) m
        Nothing -> m
    where m = Map.mapKeys (\x -> x - 1) map



parse :: [Char] -> Map.Map Int Int
parse input = foldl foldFn emptyMap list
    where list = map (read::String->Int) (splitOn "," input)
          foldFn map c = Map.adjust (+ 1) c map

six = do
        handle <- openFile "six-input.txt" ReadMode
        contents <- hGetContents handle

        let p = parse contents
        let e = iterate advanceDay p!!256
        let sum = Map.foldr (+) 0 e
        print sum
        hClose handle
