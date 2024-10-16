module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = go S.empty parsedInput
            where go seen state
                    | state `S.member `seen = [state]
                    | otherwise             = state:(go (S.insert state seen)
                                                        (reallocate state))
    let repeatedState = last states
    let answer1 = length states - 1
    let answer2 = answer1 - Common.firstIdWhere (==repeatedState) states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Banks = M.Map Int Int

parse :: String -> Banks
parse input = M.fromList . zip [0..] . map read $ splitOn "\t" input

reallocate :: Banks -> Banks
reallocate banks = let sorted = sortOn snd $ M.toList banks
                       maxBlocks = maximum $ map snd sorted
                       i = fst $ Common.firstWhere ((==maxBlocks) . snd) sorted
                    in foldr (\x -> M.insertWith (+)
                                                 (x `mod` (M.size banks))
                                                 1)
                             (M.insert i 0 banks)
                             [(i+1)..(i+maxBlocks)]
