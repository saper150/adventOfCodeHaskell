
module Nine (nine) where

import System.IO
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Debug.Trace (traceShow)
import Data.List (sort)


parse input = Map.fromList (concatMap mapFn (zip [0..] (lines input)))
        where mapFn (y,line) = zipWith (\ x v -> ((x, y), digitToInt v)) [0..] line


neabours::(Int,Int)->Map.Map (Int, Int) Int->[Int]
neabours (x, y) points = map (\yy-> Map.findWithDefault maxBound yy points) [(x+1, y), (x-1, y), (x, y +1), (x, y-1)]

neabours2::(Int,Int)->Int->Int->[(Int, Int)]
neabours2 (x, y) width height = filter filterFn [(x+1, y), (x-1, y), (x, y +1), (x, y-1)]
    where filterFn (xx, yy) = xx >= 0 && yy >= 0 && xx < width && yy < height


allPoints width height = [(x,y) | y <- [0..height-1], x<- [0..width-1]]

lowPoints::Map.Map (Int, Int) Int->Int->Int->[Int]
lowPoints points width height = map snd (Map.toList (Map.filterWithKey filterFn points))
        where _allPoints = allPoints width height
              filterFn::(Int,Int)->Int->Bool
              filterFn key val = all (val <) (neabours key points)

risk points = sum points + length points

basins::Map.Map (Int, Int) Int -> (Int,Int) -> [Int]
basins points (width,height) = startFinding (allPoints width height) Set.empty
    where startFinding [] visited = []
          startFinding (point:xs) visited
            | Set.member point visited = startFinding xs visited
            | points Map.! point == 9 = startFinding xs visited
            | otherwise = snd res: startFinding xs (fst res)
                where res = findBasin point visited 0

          findBasin point visited count
              | Set.member point visited = (visited, count)
              | otherwise = (fst fff, snd fff + 1)
               where basinNeabours = filter filterFn (neabours2 point width height)
                     filterFn p = (points Map.! p) /= 9
                     fff = foldl (\(_visited, _count) c -> findBasin c _visited _count) (Set.insert point visited, count) basinNeabours


basinsScore list = product (take 3 ((reverse . sort) list))

nine = do
        handle <- openFile "nine-input.txt" ReadMode
        contents <- hGetContents handle
        let _lines = lines contents
        let (width, height) = ((length . head) _lines, length _lines)
        let points = parse contents

        let bb = basins points (width, height)
        print bb
        print (width, height)
        print (basinsScore bb)
        -- print (neabours (1, 1) points)
        -- print (risk (lowPoints points width height))
        -- print (parse contents)
        hClose handle
