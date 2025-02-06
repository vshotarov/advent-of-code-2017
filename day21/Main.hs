module Main where

import qualified Common
import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let initState = [".#.",
                     "..#",
                     "###"]
        allStates = iterate (process parsedInput) initState
        processNTimes n = last $ take (n+1) allStates
    let answer1 = numOnPixels $ processNTimes 5
    let answer2 = numOnPixels $ processNTimes 18

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Pattern = [String]
type Rule = (Pattern,Pattern)

parse :: String -> [Rule]
parse input = map parseOne $ lines input
    where parseOne = Common.mapTuple (splitOn "/") . Common.splitOnceOn " => "

versions :: Pattern -> [Pattern]
versions a = rotations ++ map reverse rotations
    where clockwise = map reverse . transpose
          rotations = take 4 $ iterate clockwise a

match :: Pattern -> Pattern -> Bool
match a = any (a==) . versions

printFormat :: Pattern -> String
printFormat = concatMap (++"\n")

chunk :: Pattern -> [Pattern]
chunk p = concatMap toCols $ chunksOf s p
    where s = if (length p) `mod` 2 == 0 then 2 else 3
          toCols [] = []
          toCols xs | all null xs = []
          toCols xs = (map (take s) xs):(toCols $ map (drop s) xs)

join :: [Pattern] -> Pattern
join [] = error "cannot join empty list"
join [q] = q
join qs = concat $ map concatRow $ toRows qs
    where n = (floor :: (Double -> Int)) . sqrt . fromIntegral $ length qs
          toRows [] = []
          toRows b = (take n b):(toRows $ drop n b)
          concatRow r = foldr1 (\x acc -> map (uncurry (++)) $ zip x acc) r

enhance :: [Rule] -> Pattern -> Pattern
enhance rules pattern = snd . head $ filter (match pattern . fst) rules

process :: [Rule] -> Pattern -> Pattern
process rules pattern = join . map (enhance rules) $ chunk pattern

numOnPixels :: Pattern -> Int
numOnPixels = length . filter (=='#') . concat
