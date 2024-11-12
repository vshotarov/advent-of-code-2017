module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let run t = map (\(l,d) -> (l,((t+l) `mod` (d*2-2)) == 0)) $ M.toList parsedInput
    let answer1 = sum . map (\(a,b) -> (fromEnum b) * a * (parsedInput M.! a)) $ run 0
    let answer2 = Common.firstWhere (not . any snd . run) [0..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> M.Map Int Int
parse input = M.fromList
            . map ((\(a,b) -> (read a, read b)) . (Common.splitOnceOn ": "))
            $ lines input
