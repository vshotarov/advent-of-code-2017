module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(genA,genB) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let getMatches = length . filter (uncurry areLast16BitsEqual)
    let answer1 = getMatches . take 40000001
                $ iterate (\(a,b) -> (step a 'A', step b 'B')) parsedInput
    let answer2 = getMatches . take 5000000  $ zip as bs
            where filterFunc 'A' v = v `mod` 4 == 0
                  filterFunc 'B' v = v `mod` 8 == 0
                  filterFunc _ _ = error "unrecognised generator"
                  as = filter (filterFunc 'A') $ iterate (\v -> step v 'A') genA
                  bs = filter (filterFunc 'B') $ iterate (\v -> step v 'B') genB

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (Integer,Integer)
parse input = (\(a,b) -> (lastWordAsInteger a, lastWordAsInteger b))
            . Common.splitOnceOn "\n" $ init input
    where lastWordAsInteger = read . reverse . fst . Common.splitOnceOn " " . reverse 

step :: Integer -> Char -> Integer
step v 'A' = (v * 16807) `mod` remainderOfDivWith
step v 'B' = (v * 48271) `mod` remainderOfDivWith
step _ _ = error "unrecognised generator"

remainderOfDivWith :: Integer
remainderOfDivWith = 2147483647

areLast16BitsEqual :: Integer -> Integer -> Bool
areLast16BitsEqual aa bb = all (uncurry (==)) $ take 16 $ go aa bb
    where go a b = (a `mod` 2, b `mod` 2):(go (a `div` 2) (b `div` 2))
                       
