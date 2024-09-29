module Main where

import qualified Common
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = manhattan $ spiral !! (parsedInput-1)
                    where manhattan (x,y) = abs x + abs y
    let answer2 = last . Common.firstWhere ((>parsedInput) . last)
                $ iterate spiralSum [1]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> Int
parse input = read input

spiral :: [(Int,Int)]
spiral = go 1 (1,0) (0,0) 1.5
    where go 0 (dx,dy) p n'         = go (floor n') (-dy,dx) p (n'+0.5)
          go n d@(dx,dy) p@(x,y) n' = p:(go (n-1) d (x+dx,y+dy) n')

spiralSum :: [Int] -> [Int]
spiralSum s = s ++ [sum $ map (s !!) ns]
    where i = length s
          (x,y) = spiral !! length s
          ns = [ni | dx <- [-1..1], dy <- [-1..1],
                let ni = fromJust $ elemIndex (x+dx,y+dy) spiral,
                ni < i]

