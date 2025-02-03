module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = head $ drop (id2017+1) after2017
            where after2017 = fst . last . take 2018
                            $ iterate (spinlock parsedInput) ([0],(0,1))
                  id2017 = Common.firstIdWhere (==2017) after2017
    let answer2 = fst . last . take 50000000 $ iterate (spinlock2 parsedInput) (0,(0,1))

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Buffer = [Int]
type Position = Int


type State1 = (Buffer,(Position,Int))
type State2 = (Int,(Position,Int))

parse :: String -> Int
parse input = read input

-- can change `length buffer` to `v` for optim
spinlock :: Int -> State1 -> State1
spinlock _ ([0],(_,v)) = ([0,v],(1,v+1))
spinlock step (buffer,(position,v)) = ((take p buffer) ++ [v] ++ (drop p buffer),
                                       (p,v+1))
    where p = 1 + (position + step) `mod` v

spinlock2 :: Int -> State2 -> State2
spinlock2 step (_,(1,v)) = (v-1,(p,v+1)) where p = 1 + (1 + step) `mod` v
spinlock2 step (n,(position,v)) = (n,(p,v+1)) where p = 1 + (position + step) `mod` v
