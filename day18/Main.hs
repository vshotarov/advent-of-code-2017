module Main where

import qualified Common
import Data.Char (ord,chr)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = (fst (process 0 M.empty parsedInput
                    (\(state,reg,i) -> (M.insert "_played" (argValue state reg) state, i+1))
                    (\(state,_,_) -> (M.insert "_recovered" (state M.! "_played") state, -1))
                   )) M.! "_recovered"
    let state0 = M.insert "p" 0 $ M.empty
    let state1 = M.insert "p" 1 $ M.empty
    let answer2 = solve2 parsedInput state0 state1

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = M.Map String Int
type StringOrInt = Either String Int
data Instruction = UnaryOp String StringOrInt
                 | BinaryOp String StringOrInt StringOrInt
                 deriving Show
type Callback = (State,StringOrInt,Int) -> (State,Int)

isRcv :: Instruction -> Bool
isRcv (UnaryOp "rcv" _) = True
isRcv _ = False

parse :: String -> [Instruction]
parse input = map (toInstruction . words) $ lines input
    where parseArg [] = error "malformatted arg"
          parseArg arg@(a:_)
            | ord a >= 97 && ord a <= 122 = Left arg
            | otherwise = Right (read arg)
          toInstruction [op,arg1] = UnaryOp op (parseArg arg1)
          toInstruction [op,arg1,arg2] = BinaryOp op (parseArg arg1) (parseArg arg2)
          toInstruction _ = error "malformatted instruction"

argValue :: State -> StringOrInt -> Int
argValue _ (Right v) = v
argValue state (Left reg) = M.findWithDefault 0 reg state

processOne :: State -> Int -> Instruction -> Callback -> Callback -> (State,Int)
processOne state i (UnaryOp "snd" reg) sndCallback _ = sndCallback (state,reg,i)
processOne state i (BinaryOp "set" (Left x) y) _ _ = (M.insert x (argValue state y) state, i+1)
processOne state i (BinaryOp "add" (Left x) y) _ _ =
    (M.insert x ((M.findWithDefault 0 x state) + (argValue state y)) state, i+1)
processOne state i (BinaryOp "mul" (Left x) y) _ _ =
    (M.insert x ((M.findWithDefault 0 x state) * (argValue state y)) state, i+1)
processOne state i (BinaryOp "mod" (Left x) y) _ _ =
    (M.insert x ((M.findWithDefault 0 x state) `mod` (argValue state y)) state, i+1)
processOne state i (UnaryOp "rcv" reg) _ rcvCallback
-- ADDRESS THIS | argValue state reg == 0 = (state,i+1)
  | otherwise = rcvCallback (state,reg,i)
processOne state i (BinaryOp "jgz" x y) _ _
  | argValue state x <= 0 = (state, i+1)
  | otherwise = (state, i + argValue state y)
processOne _ _ op _ _ = error ("unexpected op " ++ show op)

process :: Int -> State -> [Instruction] -> Callback -> Callback -> (State, Int)
process _ _ [] _ _ = error "no instructions to process"
process startI initialState instructions sndCallback rcvCallback = go initialState startI
    where go state i
            | i<0 || i >= length instructions = (state, i)
            | otherwise = let (state',i') = processOne state i (instructions !! i)
                                                       sndCallback rcvCallback
                           in go state' i'

solve2 :: [Instruction] -> State -> State -> Int
solve2 instructions initStateA initStateB = go (0::Int) (initStateA,initStateB)
                                                        (0,0) ([],[]) False
    where sndCallback (state,reg,i) =
              (M.insert "_sent" (argValue state reg) state, -(100000+i+1))
          rcvCallback (state,(Left reg),i) =
              (M.insert "_toReceive" (ord (head reg)) state, -(200000+i+1))
          rcvCallback _ = error "cannot call rcv with an integer"
          go p (state0,state1) (i0,i1) (toRcv0,toRcv1) deadlock =
              let (state0',i0') = process i0 state0 instructions sndCallback rcvCallback
                  (state1',i1') = process i1 state1 instructions sndCallback rcvCallback
                  toRcv = if p == 0 then toRcv0 else toRcv1
                  ifCanRcvState s = M.insert ((chr (s M.! "_toReceive")):[]) (head toRcv) s
                  ifCanRcvStates = if p == 0 then (ifCanRcvState state0', state1)
                                             else (state0, ifCanRcvState state1')
                  ifCanRcvIds = if p == 0 then (-i0'-200000,i1) else (i0,-i1'-200000)
                  ifCanRcvBuffers = if p == 0 then (tail toRcv0, toRcv1) else (toRcv0, tail toRcv1)
                  ifCanRcv = go p ifCanRcvStates ifCanRcvIds ifCanRcvBuffers False
                  ifSentStates = if p == 0 then (state0',state1) else (state0,state1')
                  ifSentIds = if p == 0 then (-i0'-100000,i1) else (i0,-i1'-100000)
                  ifSentBuffers = if p == 0 then (toRcv0, toRcv1 ++ [state0' M.! "_sent"])
                                            else (toRcv0 ++ [state1' M.! "_sent"], toRcv1)
                  ifSent = go p ifSentStates ifSentIds ifSentBuffers False
                  go' q (i0'',i1'') = go q (if p == 0 then (state0',state1) else (state0,state1'))
                                           (i0'',i1'') (toRcv0,toRcv1)
                  goIds = if p == 0 then (-i0'-200000-1,i1) else (i0,-i1'-200000-1)
                  (i,j) = if p == 0 then (i0',i1) else (i1',i0)
               in if i < -200000
                     then (if null toRcv
                              then (if deadlock
                                       then 0
                                       else (if j<0 || j >= length instructions
                                                then 0
                                                else go' (1-p) goIds True))
                              else ifCanRcv)
                     else (if i < 100000
                              then (if p == 1 then 1 else 0) + ifSent
                              else (if i<0 || i >= length instructions
                                       then (if j<0 || j >= length instructions
                                                then 0
                                                else go' (1-p) (i,j) True)
                                       else (error "finished with non out of range index")))
