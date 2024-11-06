module Main where

import qualified Common
import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord)
import Data.Bits (xor)

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let process current skip lengths xs =
            foldl (\((c,s),a) l -> (((c+l+s) `mod` n,
                                     (s+1) `mod` n),
                                     processSingleStep c l a))
                  ((current,skip),xs) lengths
            where n = length xs

    let answer1 = product . take 2 . snd $ process 0 0 parsedInput [0..255]

    let answer2 = concatMap (toHex . foldr1 xor) . chunksOf 16
                . snd . last . take 65
                $ iterate (\((c,s),is) -> process c s instructions2 is)
                          ((0,0),[0..255])
            where instructions2 = toAsciiList input ++ [17,31,73,47,23]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse input = map read $ splitOn "," input

processSingleStep :: Int -> Int -> [Int] -> [Int]
processSingleStep current len is = is'
    where chunk = take len $ drop current (is ++ is)
          chunk' = reverse chunk
          overlap = max 0 $ len - (length is - current)
          noOverlap = length chunk - overlap
          is' = (take overlap $ drop noOverlap chunk')
             ++ (drop overlap $ take current is)
             ++ (take noOverlap chunk')
             ++ (drop (current + len) is)

toAsciiList :: String -> [Int]
toAsciiList = map ord

toHex :: Int -> String
toHex dec = map ("0123456789abcdef" !!) [a,b]
    where a = dec `div` 16
          b = dec `mod` 16
