module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Tests
    print "Score"
    print $ (\x -> (x, score x)) "{}"
    print $ (\x -> (x, score x)) "{{{}}}"
    print $ (\x -> (x, score x)) "{{},{}}"
    print $ (\x -> (x, score x)) "{{{},{},{{}}}}"
    print $ (\x -> (x, score x)) "{<a>,<a>,<a>,<a>}"
    print $ (\x -> (x, score x)) "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    print $ (\x -> (x, score x)) "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    print $ (\x -> (x, score x)) "{{<a!>},{<a!>},{<a!>},{<ab>}}"

    print "Garbage"
    print $ (\x -> (x, garbage x)) "<>"
    print $ (\x -> (x, garbage x)) "<random characters>"
    print $ (\x -> (x, garbage x)) "<<<<>"
    print $ (\x -> (x, garbage x)) "<{!>}>"
    print $ (\x -> (x, garbage x)) "<!!>"
    print $ (\x -> (x, garbage x)) "<!!!>>"
    print $ (\x -> (x, garbage x)) "<{o\"i!a,<{i<a>"

    -- Solve
    let answer1 = score input
    let answer2 = garbage input

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

score :: String -> Int
score input = go [] 0 input
    where go _ s [] = s
          go b s ('!':xs) = go b s $ drop 1 xs
          go b@('<':_) s ('{':xs) = go b  s xs
          go b@('<':_) s ('<':xs) = go b  s xs
          go b@('<':_) s ('}':xs) = go b  s xs
          go  ('<':b') s ('>':xs) = go b' s xs
          go b s ('<':xs) = go ('<':b) s xs
          go b s ('{':xs) = go ('{':b) s xs
          go ('{':b') s ('}':xs) = go b' (s+(length b')+1) xs
          go b s (_:xs) = go b s xs

garbage :: String -> Int
garbage input = go [] 0 input
    where go _ s [] = s
          go b s ('!':xs) = go b s $ drop 1 xs
          go ('<':b') s ('>':xs) = go b' s xs
          go b@('<':_) s (_:xs)  = go b (s+1) xs
          go b s ('<':xs) = go ('<':b) s xs
          go b s (_:xs) = go b s xs

