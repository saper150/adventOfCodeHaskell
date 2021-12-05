
module One (one) where

import System.IO
import Control.Monad


window::[Int] -> [Int]
window (xa:xb:xc:xs) = (xa + xb + xc) : window (xb:xc:xs)
window x = []

f (x:xy:xs) = (if xy > x then 1 else 0) + f(xy:xs)
f [x] = 0
f [] = 0

one = do
        handle <- openFile "one-input.txt" ReadMode
        contents <- hGetContents handle
        print (f (window (map (read::String->Int) (lines contents))))
        hClose handle
