module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = splitOn "," input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let abc = if length parsedInput < 10 then "abcde" else "abcdefghijklmnop"
    let numIters = 1000000000
    let answer1 = dance abc parsedInput
    let answer2 = last . take (1 + numIters - lastRepeatId)
                $ iterate (\x -> dance x parsedInput) repeated
            where go seen x
                    | x `S.member` seen = (x,S.size seen)
                    | otherwise = go (S.insert x seen) $ dance x parsedInput
                  (repeated,repeatId) = go S.empty abc
                  lastRepeatId = (numIters `div` repeatId) * repeatId

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

dance :: String -> [String] -> String
dance abc [] = abc
dance abc (('s':chunk):steps) = dance ((drop before abc) ++ take before abc) steps
    where before = (length abc - read chunk)
dance abc (('x':chunk):steps) = dance (map swapMap abc) steps
    where (a,b) = Common.mapTuple read $ Common.splitOnceOn "/" chunk :: (Int,Int)
          swapMap c
            | c == abc !! a = abc !! b
            | c == abc !! b = abc !! a
            | otherwise     = c
dance abc (('p':a:'/':b:_):steps) = dance (map swapMap abc) steps
    where swapMap c
            | c == a    = b
            | c == b    = a
            | otherwise = c
dance _ (s:_) = error ("Unrecognised dance step" ++ s)
