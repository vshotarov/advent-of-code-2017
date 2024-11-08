module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    mapM_ ((\i -> print (i, manhattan (0,0,0) $ foldl step (0,0,0) $ parse i)))
         ["ne,ne,ne","ne,ne,sw,sw","ne,ne,s,s","se,sw,se,sw,sw"]

    let path = scanl step (0,0,0) parsedInput
    let answer1 = manhattan (0,0,0) $ last path 
    let answer2 = maximum $ map (manhattan (0,0,0)) path

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [String]
parse input = splitOn "," input


step :: (Int,Int,Int) -> String -> (Int,Int,Int)
step (x,y,z) "n"  = (x,y+1,z-1)
step (x,y,z) "s"  = (x,y-1,z+1)
step (x,y,z) "ne" = (x+1,y,z-1)
step (x,y,z) "nw" = (x-1,y+1,z)
step (x,y,z) "se" = (x+1,y-1,z)
step (x,y,z) "sw" = (x-1,y,z+1)
step _        d   = error ("Unrecognised direction " ++ d)

manhattan :: (Int,Int,Int) -> (Int,Int,Int) -> Int
manhattan (x1,y1,z1) (x2,y2,z2) =
    ((abs $ x1 - x2) + (abs $ y1 - y2) + (abs $ z1 - z2)) `div` 2
