module Main where

import qualified Common
import qualified Data.Map as M
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let maximums = map maximum . filter (not . null) $ scanl step M.empty parsedInput
    let answer1 = last maximums
    let answer2 = maximum maximums

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[String]]
parse input = map words $ lines input

step :: M.Map String Int -> [String] -> M.Map String Int
step state (r:op:x:_:ca:c:cb:[]) = if eval_cond c ca' cb'
                                      then M.insert r r' state
                                      else state
    where ca' = if ord (head ca) < 97 then read ca else M.findWithDefault 0 ca state
          cb' = if ord (head cb) < 97 then read cb else M.findWithDefault 0 cb state
          op' = if op == "inc" then (+) else (-)
          eval_cond ">"  a b = a >  b
          eval_cond "<"  a b = a <  b
          eval_cond ">=" a b = a >= b
          eval_cond "<=" a b = a <= b
          eval_cond "==" a b = a == b
          eval_cond "!=" a b = a /= b
          eval_cond cond _ _ = error ("Unrecognised condition " ++ cond)
          r' = op' (M.findWithDefault 0 r state) (read x)
step _ ins = error ("Unrecognised instruction " ++ concatMap (++ " ") ins)
