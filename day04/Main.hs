module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter isValid1 parsedInput
            where isValid1 = isValid id
    let answer2 = length $ filter isValid2 parsedInput
            where isValid2 = isValid sort

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [String]
parse input = lines input

type InsertRemapper = String -> String
type Password = String

isValid :: InsertRemapper -> Password -> Bool
isValid remapper password = go [] $ splitOn " " password
    where go _      []     = True
          go buffer (x:xs)
            | (remapper x) `elem` buffer = False
            | otherwise                  = go ((remapper x):buffer) xs
