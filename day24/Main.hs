module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let strength = sum . map (uncurry (+))
    let answer1 = strength . head $ componentDfs strength parsedInput
    let answer2 = maximum . map strength $ componentDfs length parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Component = (Int,Int)

parse :: String -> [Component]
parse input = map (Common.mapTuple read . Common.splitOnceOn "/") $ lines input

componentDfs :: ([Component] -> Int) -> [Component] -> [[Component]]
componentDfs scoreF components = go [[(0,0)]] exploreLists
    where exploreLists = [(scoreF [(a,b)],
                           [if a == 0 then (b,a) else (a,b)],
                           [components !! r | r <- [0..length components-1], r /= c])
                          | c <- [0..length components-1],
                            let (a,b) = components !! c,
                            a == 0 || b == 0]
          updateBest best current
            | scoreF current == (scoreF $ head best) = (current:best)
            | scoreF current > (scoreF $ head best) = [current]
            | otherwise = best
          go best [] = best
          go best ((score,bridge@((pins,_):_),remaining):others)
            | scoreF (head best) > scoreF bridge + scoreF remaining = go best others
            | otherwise = go (updateBest best bridge)
                        $ [(score + scoreF [(a,b)],
                            (if a == pins then (b,a) else (a,b)):bridge,
                            remaining')
                           | i <- [0..length remaining-1],
                             let (a,b) = remaining !! i
                                 remaining' = [remaining !! j
                                               | j <- [0..length remaining-1],
                                                 j /= i],
                             a == pins || b == pins
                          ] ++ others
          go _ _ = error "Explore list contains empty bridges"
