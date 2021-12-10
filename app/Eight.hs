
module Eight (eight) where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import Data.Foldable (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

forceJust (Just x) = x

parse input = map ( (\[a,b] -> (a, b)) . map words . splitOn "|") (lines input)




solveLine line = initialMap
        where findLength l = forceJust (find (\x-> length x == l) line)
              one = findLength 2
              four = findLength 4
              seven = findLength 3
              mapInsert::[Char]->[Char]->Map.Map Char (Set.Set Char)->Map.Map Char (Set.Set Char)
              mapInsert keys list map = foldl (\acc c-> Map.insertWith Set.intersection c (Set.fromList list) acc) map keys
              initialMap = (
                Map.adjust (\x -> Set.difference x (Set.fromList one)) 'b'
                . Map.adjust (\x -> Set.difference x (Set.fromList one)) 'd'
                . Map.adjust (\x -> Set.difference x (Set.fromList seven)) 'd'
                . Map.adjust (\x -> Set.difference x (Set.fromList seven)) 'b'
                . Map.adjust (\x -> Set.difference x (Set.fromList four)) 'a'
                . Map.adjust (\x -> Set.difference x (Set.fromList one)) 'a'
                . mapInsert ['f', 'c'] one
                . mapInsert ['c', 'f', 'd', 'b'] four
                . mapInsert ['a', 'c', 'f'] seven
                ) Map.empty
              aValue = Set.elemAt 0 (initialMap Map.! 'a')
              secondMap = Map.map (Set.delete aValue) initialMap




countC :: (Foldable t1, Foldable t2) => t2 (a1, [t1 a2]) -> [t1 a2]
countC input = filter filterFn (concatMap snd input)
        where filterFn x = len == 7 || len == 2 || len == 4 || len == 3
                where len = length x



eight = do
        handle <- openFile "eight-input.txt" ReadMode
        contents <- hGetContents handle
        let parsed = parse contents
        print (solveLine (fst (head parsed)))
        -- print (map (solveLine . fst) parsed)
        -- print parsed
        -- print (length (countC parsed))
        hClose handle
