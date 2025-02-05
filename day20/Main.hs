module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn,nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let allStates = iterate tickAll parsedInput
    let answer1 = solve f 500 allStates
            where f = (fst . head . sortOn snd . zip [0..] . map (manhattan0 . position))
    let answer2 = solve (\s -> length s) 500 $ removeCollisions [0..length parsedInput-1] allStates
            where 


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int,Int)
type Particle = (Vec,Vec,Vec)

position :: Particle -> Vec
position (p,_,_) = p

acceleration :: Particle -> Vec
acceleration (_,_,a) = a

manhattan0 :: Vec -> Int
manhattan0 (x,y,z) = abs x + abs y + abs z

mapVec :: (Int -> Int) -> Vec -> Vec
mapVec f (x,y,z) = (f x, f y, f z)

positionAfterT :: Int -> Particle -> Vec
positionAfterT t ((px,py,pz),(vx,vy,vz),(ax,ay,az)) =
    (afterT px vx ax, afterT py vy ay, afterT pz vz az)
    where afterT p v a = (2*p + 2*v*t + a*t*(t+1)) `div` 2

parse :: String -> [Particle]
parse input = map (listToTuple3 . parseOne) $ lines input
    where skip1 [] = []
          skip1 [a] = [a]
          skip1 (a:_:xs) = a:(skip1 xs)
          listToTuple3 [x,y,z] = (x,y,z)
          listToTuple3 _ = error "cannot convert list of non-3 size to tuple3"
          parseOne line = map (\x -> listToTuple3 . map read $ splitOn ","
                                    (if head x == ' ' then tail x else x))
                        . skip1 $ concatMap (splitOn ">") . tail $ splitOn "<" line

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (x:xs) = (if a == x then 1 else 0) + count a xs

tick :: Particle -> Particle
tick (p,v,a) = ((add p v'),v',a)
    where add (x,y,z) (i,j,k) = (x+i,y+j,z+k)
          v' = add v a

tickAll :: [Particle] -> [Particle]
tickAll = map tick

solve :: ([a] -> Int) -> Int -> [[a]] -> Int
solve f itersWithoutChangeToExit states = go [] states
    where exitCond b = length b > itersWithoutChangeToExit
                    && (length . nub $ take itersWithoutChangeToExit b) == 1
          go b xs | exitCond b = head b
                  | otherwise = go ((f (head xs)):b) $ tail xs

removeCollisions :: [Int] -> [[Particle]] -> [[Int]]
removeCollisions alive states = alive':(removeCollisions alive' $ tail states)
    where positions = map position $ head states
          collisions = filter (\p -> count p positions > 1) positions
          alive' = filter (not . (`elem` collisions) . (positions !!)) alive
