module Main where

import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"

    -- I've never NOT parsed the input, but just hardcoded the instructions,
    -- so this is a nice precedent to try that.

    -- Solve
    let answer1 = sum $ tapeAfterN (12629077 :: Int) ('A',M.fromList [(0,0)],0)
            where tapeAfterN 0 (_,t,_) = t
                  tapeAfterN n s = tapeAfterN (n-1) $ step s
    let answer2 = "not solved yet"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Tape = M.Map Int Int
type Pointer = Int
type State = Char

write :: Tape -> Pointer -> Int -> Tape
write tape i value = M.insert i value tape

step :: (State, Tape, Pointer) -> (State, Tape, Pointer)
step (state,tape,i) = if ((M.findWithDefault 0 i tape) == 1) then next1 else next0
    where write' = write tape i
          next 'A' = (('B',write' 1,i+1),('B',write' 0,i-1))
          next 'B' = (('C',write' 0,i+1),('B',write' 1,i-1))
          next 'C' = (('D',write' 1,i+1),('A',write' 0,i-1))
          next 'D' = (('E',write' 1,i-1),('F',write' 1,i-1))
          next 'E' = (('A',write' 1,i-1),('D',write' 0,i-1))
          next 'F' = (('A',write' 1,i+1),('E',write' 1,i-1))
          next _ = error ("Unrecognised state " ++ [state])
          (next0,next1) = next state
