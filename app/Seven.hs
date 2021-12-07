
module Seven (seven) where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (sort)

parse input = map (read::String->Int) (splitOn "," input)

fuelConsumption input i = foldl foldFn 0 input
    where foldFn acc c = acc + sumInt (abs (c - i))
          sumInt n = (n+1)*n `div` 2

seven = do
        handle <- openFile "seven-input.txt" ReadMode
        contents <- hGetContents handle
        let parsed = parse contents
        let f = minimum $ map (fuelConsumption parsed) [0..maximum parsed]
        print f

        hClose handle
