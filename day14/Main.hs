module Main where

import qualified Common
import Data.List.Split (chunksOf)
import Data.Char (ord)
import Data.Bits (xor)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let grid = map (concatMap (zeroPad4 . decToBin . hexToDec)
                  . knotHash . (parsedInput ++) . ("-" ++) . show) [0..127]
    let answer1 = sum $ map (length . filter (==1)) grid
    let answer2 = findNumIslands grid

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

knotHash :: String -> String
knotHash x = concatMap (toHex . foldr1 xor) . chunksOf 16 . snd . last . take 65
           $ iterate (\((c,s),is) -> process c s (toAsciiList x ++ [17,31,73,47,23]) is)
                     ((0,0),[0..255])
    where process current skip lengths xs =
            foldl (\((c,s),a) l -> (((c+l+s) `mod` n,
                                     (s+1) `mod` n),
                                     processSingleStep c l a))
                  ((current,skip),xs) lengths
            where n = length xs
          toAsciiList = map ord
          toHex dec = map ("0123456789abcdef" !!) [a,b]
              where a = dec `div` 16
                    b = dec `mod` 16
          processSingleStep current len is = is'
              where chunk = take len $ drop current (is ++ is)
                    chunk' = reverse chunk
                    overlap = max 0 $ len - (length is - current)
                    noOverlap = length chunk - overlap
                    is' = (take overlap $ drop noOverlap chunk')
                       ++ (drop overlap $ take current is)
                       ++ (take noOverlap chunk')
                       ++ (drop (current + len) is)

hexToDec :: Char -> Int
hexToDec x
  | ord x > 57 = 10 + (ord x) - 97
  | otherwise  = (ord x) - 48

decToBin :: Int -> [Int]
decToBin x | x > 15 = error ""
decToBin 0 = [0]
decToBin a = go [] a
    where go b 0 = b
          go b x = go ((if (x `mod` 2 == 1) then 1 else 0):b) (x `div` 2 )

zeroPad4 :: [Int] -> [Int]
zeroPad4 x = (take (max 0 (4-(length x))) $ repeat 0) ++ x

findNumIslands :: [[Int]] -> Int
findNumIslands grid = go S.empty $ (\(_,_,x) -> [x]) $ findNextStart S.empty
    where (maxX,maxY) = (length $ head grid, length grid)
          positions = concatMap (\y -> map (\x -> (x,y)) [0..maxX-1]) [0..maxY-1]
          findNextStart seen = (length positions == length gap,seen',nextP)
                where gap = takeWhile (\(x,y) -> ((grid !! y) !! x == 0)
                                                 || ((x,y) `S.member` seen))
                                      positions
                      nextP = positions !! (length gap)
                      seen' = S.union seen $ S.fromList gap
          go seen []
            | S.size seen == maxX * maxY = 1
            | otherwise = if done
                             then 1
                             else 1 + (go seen' [nextP])
                                 where (done,seen',nextP) = findNextStart seen
          go seen ((x,y):ps)
            | (x,y) `S.member` seen = go seen ps
            | x < 0 || x >= maxX || y < 0 || y >= maxY = go seen ps
            | (grid !! y) !! x == 0 = go (S.insert (x,y) seen) ps
            | otherwise = go (S.insert (x,y) seen)
                           $ ps ++ ns
                            where ns = [(x+x',y+y') | x' <- [-1,0,1],
                                                  y' <- [-1,0,1],
                                                  x' == 0 || y' == 0,
                                                  x' /= y']
