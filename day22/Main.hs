module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states1 = take 10000 $ iterate burst1 (0,(parsedInput,((0,0),(0,-1))))
    let answer1 = fst $ last states1
    let states2 = take 10000001 $ iterate burst2 (0,(parsedInput,((0,0),(0,-1))))
    let answer2 = fst $ last states2

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

    --mapM_ (putStrLn . display . snd) $ take 5 states2

type Vec = (Int,Int)
type Grid = M.Map Vec Int
type State = (Grid,(Vec,Vec))

parse :: String -> Grid
parse input = M.fromList [((x-zero,y-zero),2 * (fromEnum $ '#' == (asLines !! y) !! x))
                          | x <- [0..size-1], y <- [0..size-1]]
    where asLines = lines input
          size = length asLines
          zero = size `div` 2

infected :: Grid -> Vec -> Bool
infected s (x,y) = 2 == M.findWithDefault 0 (x,y) s

burst1 :: (Int,State) -> (Int,State)
burst1 (c,(grid,(p@(x,y),(dx,dy)))) = (c + (fromEnum $ old == 0),
                                       (grid',((x+dx',y+dy'),d')))
    where d'@(dx',dy') = if infected grid p then ((-dy,dx)) else ((dy,-dx) )
          old = M.findWithDefault 0 p grid
          grid' = if old == 2 then (M.delete p grid) else (M.insert p 2 grid)

burst2 :: (Int,State) -> (Int,State)
burst2 (c,(grid,(p@(x,y),(dx,dy)))) = (c + (fromEnum $ old == 1),
                                       (grid',((x+dx',y+dy'),d')))
    where d'@(dx',dy') = [(dy,-dx),(dx,dy),(-dy,dx),(-dx,-dy)] !! (M.findWithDefault 0 p grid)
          old = M.findWithDefault 0 p grid
          grid' = if old == 3 then (M.delete p grid) else (M.insert p (old+1) grid)

display :: State -> String
display (g,(p,d)) =
    concatMap (++"\n") [[M.findWithDefault '.' (x,y) g' | x <- [minX..maxX]]
                       | y <- [minY..maxY]]
    where keys = M.keys g
          minX = minimum $ map fst keys
          minY = minimum $ map snd keys
          maxX = maximum $ map fst keys
          maxY = maximum $ map snd keys
          ds = M.fromList [((-1,0),'<'),((1,0),'>'), ((0,1),'v'),((0,-1),'^')]
          g' = M.insert p (ds M.! d) $ M.map (\v -> ".w#f" !! v) g
