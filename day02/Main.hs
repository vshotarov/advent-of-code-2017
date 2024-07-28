module Main where

import qualified Common
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map checksum1 parsedInput
    let answer2 = sum $ map checksum2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map (map read . splitOn "\t") $ lines input

checksum1 :: [Int] -> Int
checksum1 = go Nothing Nothing
    where assign _ Nothing v    = Just v
          assign f (Just old) v = Just (f old v)
          go (Just low) (Just high) [] = high - low
          go _ _ []                    = error "checksum without min or max val"
          go low high (x:xs)           = go (assign min low x) (assign max high x) xs

factors :: Int -> [Int]
factors 0 = []
factors 1 = [1]
factors x = filter ((==0) . (mod x)) [1..x]

checksum2 :: [Int] -> Int
checksum2 = go S.empty M.empty
    where go _ _ [] = error "no divisable pairs"
          go a b (x:xs) = let fs = factors x
                              xa = S.toList . S.intersection a $ S.fromList fs
                              xb = M.lookup x b
                              a' = S.insert x a
                              b' = foldr (\f -> M.insert f x) b fs
                           in case xb of
                                Just f -> f `div` x
                                Nothing -> if null xa then go a' b' xs
                                                      else x `div` (head xa)
