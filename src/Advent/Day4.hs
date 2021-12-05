-- |
-- Module      : Advent.Day4
-- Description : Day 4 Solutions

-- <https://adventofcode.com/2021/day/4>

module Advent.Day4 where

import           Advent.Prelude

day4 :: IO ()
day4 = do
  putStrLn "day 4"
  input <- parse <$> load "day4.txt"
  print $ uncurry day4a input
  print $ uncurry day4b input

parse :: [String] -> ([String], [[String]])
parse (nums:bs) = (toCall, boards)
  where
    toCall = splitOn "," nums
    boards = chunksOf 5 $ filter (/= "") bs

day4Example :: [String]
day4Example =
  [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
  , ""
  , "22 13 17 11 0"
  , "8 2 23 4 24"
  , "21 9 14 16 7"
  , "6 10 3 18 5"
  , "1 12 20 15 19"
  , ""
  , "3 15 0 2 22"
  , "9 18 13 17 5"
  , "19 8 7 25 23"
  , "20 11 10 24 4"
  , "14 21 16 12 6"
  , ""
  , "14 21 17 24 4"
  , "10 16 15 9 19"
  , "18 8 23 26 20"
  , "22 11 13 6 5"
  , "2 0 12 3 7"
  ]

-- | Solve Day 4 Part One
-- >>> uncurry day4a (parse day4Example)
-- 4512
day4a :: [String] -> [[String]] -> Int
day4a toCall boards = solve winning called
  where
    (winning, called) = go toCall [] []
    go (n:xs) acc winners =
      case find (\b -> winner acc b) boards of
        Just board -> ([board], acc)
        Nothing -> go xs (n:acc) winners
    go [] acc winners = error "nobody won"

winner :: [String] -> [String] -> Bool
winner ns board = any (all (`elem` ns)) (rows <> cols)
  where
    rows = fmap words board
    cols = transpose rows

solve :: [[String]] -> [String] -> Int
solve winning called =
  (sum . asInts $ (concatMap words (last winning)) \\ called) * read (head called)

-- | Solve Day 4 Part Two
-- >>> uncurry day4b (parse day4Example)
-- 1924
day4b :: [String] -> [[String]] -> Int
day4b toCall boards = solve winning called
  where
    (winning, called) = go toCall [] []
    go _ (_:acc) winners | length winners == length boards = (winners, acc)
    go (n:xs) acc winners =
      case getWinners acc (boards \\ winners) of
        [] -> go xs (n:acc) winners
        wins ->
          go xs (n:acc) (winners <> wins)
    go [] _ _ = error "somebody didn't win"
    getWinners ns = foldl' go []
      where go acc b = if winner ns b then acc <> [b] else acc
