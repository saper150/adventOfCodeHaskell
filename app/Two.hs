{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Two (two) where

import Control.Monad
import System.IO

data State = State
  { x :: Int,
    y :: Int,
    aim :: Int
  }
  deriving (Show)

execute::State -> (String, Int) -> State
execute state ("forward", by) = state { x = x state + by, y = y state + aim state * by  }
execute state ("down", x) = state { aim = aim state + x }
execute state ("up", x) = state { aim = aim state - x }

ex = foldl execute (State { x = 0, y = 0, aim = 0 })

parseLine :: [String] -> (String, Int)
parseLine [operation, value] = (operation, (read :: String -> Int) value)

parse input = map (parseLine . words) (lines input)

two = do
  handle <- openFile "two-input.txt" ReadMode
  contents <- hGetContents handle
  let xx = ex (parse contents)
  print (x xx * y xx)
  hClose handle
