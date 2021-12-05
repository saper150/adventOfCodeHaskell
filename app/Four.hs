{-# LANGUAGE TupleSections #-}

module Four (four) where

import System.IO
import Control.Monad
import Data.List.Split
import qualified Data.Map as Map
import Data.List (findIndex, transpose, (\\))
import Debug.Trace (traceShow)
import Data.Maybe


type Boards = Map.Map (Int, Int, Int) (Int, Bool)

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

parseLine::Int->Int->String->[((Int, Int, Int), (Int, Bool))]
parseLine boardIndex lineIndex line = map mapFn (zip [0..] (words line))
    where mapFn = \(index, word) -> ((boardIndex, lineIndex, index), ((read::String->Int) word, False))


parseBoards [] index = []
parseBoards lines index = board ++ parseBoards rest (index + 1)
    where nextBoardStart = dropWhile (== "") lines
          (boardChuk, rest) = splitAtFirst "" nextBoardStart
          board = concatMap (\(lineIndex, line)-> (parseLine index lineIndex line)) (zip [0..] boardChuk)


parse input = (parseBoards (tail _lines) 0, moves, length _lines `div` 6)
    where _lines = lines input
          moves = map (read::String->Int) (splitOn "," (head _lines))


boardMap::String -> (Boards, [Int], Int)
boardMap input = (Map.fromList boardElements, moves, boardsCount)
    where (boardElements, moves, boardsCount) = parse input



applyMove::Int->Boards->Boards
applyMove move = Map.map mapFn
    where mapFn (num, selected)
            | num == move = (num, True)
            | otherwise = (num, selected)


-- checkBoard::Boards->Maybe Int
-- checkBoard boards = Nothing
--     where checkBoard boardIndex = map (\x-> map (\y-> (x, y) [0..4])) [0..4]

boardKeys = map (\x-> map (x,) [0..4]) [0..4]

boardCheckKeys :: [[(Int, Int)]]
boardCheckKeys = boardKeys ++ transpose boardKeys


checkBoards::Int->Boards->Maybe Int
checkBoards boardsCount boards = findIndex checkBoard [0..(boardsCount - 1)]
    where checkBoard boardIndex = any (checkRow boardIndex) boardCheckKeys
          checkRow boardIndex row = all (\(x,y) -> snd (boards Map.! (boardIndex, x, y)) ) row

checkBoardsList::[Int]->Boards->[Int]
checkBoardsList indexes boards = filter checkBoard indexes
    where checkBoard boardIndex = any (checkRow boardIndex) boardCheckKeys
          checkRow boardIndex row = all (\(x,y) -> snd (boards Map.! (boardIndex, x, y)) ) row


boardScore::Int->Boards->Int
boardScore boardIndex boards = foldl foldFn 0 (concat boardKeys)
    where foldFn acc (x, y)
            | snd element = acc
            | otherwise = fst element + acc
                where element = boards Map.! (boardIndex, x, y)

winningBoardScore::Int->Boards->[Int]->Int
winningBoardScore boardsCount boards [] = error "No winnong boards"
winningBoardScore boardsCount boards (move:rest) = case winningBoard of
    Nothing -> winningBoardScore boardsCount newBoards rest
    Just boardIndex -> boardScore boardIndex newBoards * move
    where newBoards = applyMove move boards
          winningBoard = checkBoards boardsCount newBoards


lastWinningBoardScore::Boards->[Int]->[Int]->Maybe Int->Int->Maybe Int
lastWinningBoardScore boards [] remainingBoards lastWinning lastMove = lastWinning
lastWinningBoardScore boards moves [] lastWinning lastMove = fmap (\x -> boardScore x boards * lastMove) lastWinning
lastWinningBoardScore boards (move:rest) remainingBoards lastWinning lastMove = lastWinningBoardScore newBoards rest (remainingBoards \\ winningBoards) lastWinning move

    where newBoards = applyMove move boards
          winningBoards = checkBoardsList remainingBoards newBoards
          lastWinning = if not (null winningBoards) then Just (last winningBoards) else lastWinning



four = do
        handle <- openFile "four-input.txt" ReadMode
        contents <- hGetContents handle
        let (boards, moves, boardsCount) = boardMap contents
        -- let app = foldl (\acc c -> applyMove c acc) boards [17,15,23,13,12]
        -- print (winningBoardScore boardsCount boards moves)
        let w = lastWinningBoardScore boards moves [0..boardsCount - 1] Nothing 0
        print w
        hClose handle
