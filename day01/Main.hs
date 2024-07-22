module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Test
    Common.doAssert (solve1 [1,1,2,2] == 3) "first test doesnt pass part 1"
    Common.doAssert (solve1 [9,1,2,1,2,1,2,9] == 9) "second test doesnt pass part 1"
    Common.doAssert (solve2 [1,2,1,2] == 6) "first test doesnt pass part 2"

    -- Solve
    let answer1 = solve1 parsedInput
    let answer2 = solve2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse = map (read . (:[])) . head . lines

sumEqualPairs :: [Int] -> [Int] -> Int
sumEqualPairs [] _ = 0
sumEqualPairs _ [] = 0
sumEqualPairs (a:as) (b:bs)
  | a == b    = a + sumEqualPairs as bs
  | otherwise = 0 + sumEqualPairs as bs

solve1 :: [Int] -> Int
solve1 [] = 0
solve1 (x:xs) = sumEqualPairs (x:xs) (xs ++ [x])

solve2 :: [Int] -> Int
solve2 xs = 2 * sumEqualPairs (take half xs) (drop half xs)
    where half = (length xs) `div` 2
