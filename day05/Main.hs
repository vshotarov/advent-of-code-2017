module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = walkOut (\_ -> 1) parsedInput
    let answer2 = walkOut (\x -> if x > 2 then (-1) else 1) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse input = map read $ lines input

type IncrementFunc = Int -> Int
walkOut :: IncrementFunc -> [Int] -> Int
walkOut incFunc instructions = go 0 0 . M.fromList $ zip [0..] instructions
    where go c i m = case M.lookup i m of
                       Nothing -> c
                       Just x  -> go (c+1) (i+x) $ M.insertWith (+) i (incFunc x) m
