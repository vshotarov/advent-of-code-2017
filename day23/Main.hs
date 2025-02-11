module Main where

import qualified Common
import Data.Char (ord)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = solve1 0 M.empty parsedInput
    let answer2 = "Solved on paper, but it boils down to finding the number of "
               ++ "composite (i.e. non-prime) numbers in the sequence "
               ++ "[b,b+17,b+2*17,...c], where c is b+17*1000, so the sequence "
               ++ "is exactly 1000 numbers long and - as expected - most of them "
               ++ "are not prime."

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = M.Map String Int
type StringOrInt = Either String Int
type Instruction = (String,StringOrInt,StringOrInt)

parse :: String -> [Instruction]
parse input = map (toInstruction . words) $ lines input
    where parseArg [] = error "malformatted arg"
          parseArg arg@(a:_)
            | ord a >= 97 && ord a <= 122 = Left arg
            | otherwise = Right (read arg)
          toInstruction [op,arg1,arg2] = (op,(parseArg arg1),(parseArg arg2))
          toInstruction _ = error "malformatted instruction"

argValue :: State -> StringOrInt -> Int
argValue _ (Right v) = v
argValue state (Left reg) = M.findWithDefault 0 reg state

solve1One :: State -> Int -> Int -> Instruction -> (Int,State,Int)
solve1One state i c (op, a, b) =
    case (op,a) of
      ("set",Left r) -> (i+1, M.insert r bv state, c)
      ("sub",Left r) -> (i+1, M.insert r (av-bv) state, c)
      ("mul",Left r) -> (i+1, M.insert r (av*bv) state, c+1)
      ("jnz",_) -> (if av == 0 then (i+1) else (i+bv),state,c)
      _ -> error ("unrecognised op" ++ show op)
    where av = argValue state a
          bv = argValue state b

solve1 :: Int -> State -> [Instruction] -> Int
solve1 _ _ [] = error "no instructions to solve1"
solve1 initialI initialState instructions = go initialState initialI 0
    where go state i muls
            | i<0 || i >= length instructions = muls
            | otherwise = let (i',state',muls') = solve1One state i muls (instructions !! i)
                           in go state' i' muls'
