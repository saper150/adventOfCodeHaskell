
module Three (three) where
import Debug.Trace
import System.IO
import Control.Monad
import Data.List

toDec::[Int] -> Int
toDec x = foldl (\acc (index, val) -> acc + 2^index * val) 0 (zip [0..] (reverse x))

rev::[Int] -> [Int]
rev = map (\x -> if x == 1 then 0 else 1)



execute::[[Int]] -> Int
execute l = toDec binary * toDec (rev binary)
    where transposed = transpose l
          sums = map sum transposed
          len = length (head transposed)
          binary = map (\x -> if x > div len 2 then 1 else 0) sums


filterIndex::[[Int]] -> Int -> Int -> [Int]
filterIndex [x] index ii = x
filterIndex numbers index ii = filterIndex (filter filterFn numbers) (index + 1) ii
    where filterFn = \x -> x!!index == mask
          transposed = transpose numbers
          mask
            | sum (transposed!!index) >= length numbers - sum (transposed!!index) = if ii == 0 then 0 else 1
            | ii == 0 = 1
            | otherwise = 0



execute2::[[Int]] -> Int
execute2 l = toDec (filterIndex l 0 0) * toDec (filterIndex l 0 1)





parseLine::String -> [Int]
parseLine = map ((read::String->Int). (: []))

parse input = map parseLine (lines input)


three = do
        handle <- openFile "three-input.txt" ReadMode
        contents <- hGetContents handle
        print (execute2 (parse contents))
        hClose handle
