module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ toGroup parsedInput "0"
    let answer2 = length
                $ takeWhile (not . null . M.keys)
                $ iterate (\n -> foldr (\x -> M.delete x) n
                               $ toGroup n $ head $ M.keys n) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> M.Map String [String]
parse input = foldr (\(x,ys) acc -> foldr (\x2 -> M.insertWith (++) x2 [x]
                                                . M.insertWith (++) x [x2])
                                          acc $ splitOn ", " ys) M.empty
            $ map (Common.splitOnceOn " <-> ") $ lines input

toGroup :: M.Map String [String] -> String -> [String]
toGroup network startId = go S.empty [startId]
    where go seen [] = S.toList seen
          go seen (x:xs)
            | x `S.member` seen = go seen xs
            | otherwise = go (S.insert x seen) ((network M.! x) ++ xs)
