module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = name parsedInput
    let answer2 = (\(n,w) -> w + (weight $ Common.firstWhere ((==n) . name) $ flattenTree parsedInput))
                $ (\(w,ws,n) -> (n,Common.firstWhere (/=0) $ map (\ww -> ww - w) ws))
                $ getUnbalancedChildOf
                $ Common.firstWhere ((>1) . length . nub . map weight . children)
                $ reverse $ flattenTree $ accumulateWeight parsedInput
                    where getUnbalancedChildOf node = let weights = map weight $ children node
                                                          names = map name $ children node
                                                       in Common.firstWhere (\(w,ws,_) -> length (filter (==w) ws) == 1)
                                                        $ map (\(w,n) -> (w,weights,n)) $ zip weights names

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Tree = Tree {name :: String, weight :: Int, children :: [Tree]}
    deriving (Show,Eq)

parseLine :: String -> (Tree, [String])
parseLine line = (Tree name'  (read . tail $ init weightRaw) [], children')
    where (left,right) = Common.splitOnceOn " -> " line
          (name',weightRaw) = Common.splitOnceOn " " left
          children' = filter (not . null) $ splitOn ", " right


insertInTree :: Tree -> String -> Tree -> Tree
insertInTree node parent tree
  | name node == parent = error "cannot parent a node to itself"
  | name node == name tree = error "gone too deep"
  | name tree == parent = Tree (name tree) (weight tree) (node:(children tree))
  | otherwise = Tree (name tree) (weight tree) (map (insertInTree node parent) (children tree))

getRemoveDirectChild :: Tree -> String -> (Tree,Tree)
getRemoveDirectChild (Tree parentName weight' children') childName =
    let child = Common.firstWhere ((==childName) . name) children'
     in (child, Tree parentName weight' $ filter (/=child) children')

accumulateWeight :: Tree -> Tree
accumulateWeight (Tree n w children') =
    let accumulatedChildren = map accumulateWeight children'
     in (Tree n (w + (sum $ map weight accumulatedChildren)) accumulatedChildren)

flattenTree :: Tree -> [Tree]
flattenTree tree = [tree] ++ concatMap flattenTree (children tree)

parse :: String -> Tree
parse input = head . children
            $ foldr (\(c,p) acc -> let (n,acc') = getRemoveDirectChild acc c
                                    in insertInTree n p acc')
                    nodes $ M.toList parents
    where foldLine line (tree, parents') =
            let (node,childrenNames) = parseLine line
             in (Tree (name tree) (weight tree) (node:(children tree)),
                 foldr (\x -> M.insert x (name node)) parents' childrenNames)
          (nodes,parents) = foldr foldLine (Tree "_root_" 0 [], M.empty) $ lines input
