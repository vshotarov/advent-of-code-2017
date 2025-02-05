module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let startX = Common.firstIdWhere (=='|') (head parsedInput)
        (path,steps) = follow parsedInput (startX,0)
    let answer1 = path
    let answer2 = steps

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Diagram = [String]

parse :: String -> Diagram
parse input = lines input

follow :: Diagram -> Point -> (String, Int)
follow diagram startPoint = go startPoint (0,1) ([],0)
    where get (x,y)
            | y < 0 || y >= length diagram
           || x < 0 || x >= length (diagram !! y) = ' '
            | otherwise = (diagram !! y) !! x
          go (x,y) _ r | x < 0 || y < 0
                      || y >= length diagram || x >= length (diagram !! y)
                      || get (x,y) == ' ' = r
          go p@(x,y) d@(dx,dy) (letters,steps)
            | get p == ' ' = (letters,steps)
            | not $ (get p) `elem` "-|+" = go (x+dx,y+dy) d (letters ++ [get p],steps+1)
            | get p == '+' || get (x+dx,y+dy) == ' '=
                if get (x-dy,y+dx) == ' ' then go (x+dy,y-dx) (dy,-dx) (letters,steps+1)
                                          else go (x-dy,y+dx) (-dy,dx) (letters,steps+1)
            | otherwise = go (x+dx,y+dy) (dx,dy) (letters,steps+1)
