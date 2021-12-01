-- |
-- Module      : Advent.Day1
-- Description : Day 1 Solutions

-- <https://adventofcode.com/2021/day/1>

module Advent.Day1 where

import           Advent.Prelude

day1 :: IO ()
day1 = do
  putStrLn "day 1"
  input <- asInts <$> load "day1.txt"
  print $ day1a input
  print $ day1b input

day1Example :: [Int]
day1Example =
  [ 199
  , 200
  , 208
  , 210
  , 200
  , 207
  , 240
  , 269
  , 260
  , 263
  ]

-- | Solve Day 1 Part One
-- Count the number of times a depth measurement increases from the previous measurement.
-- >>> day1a day1Example
-- 7
day1a :: [Int] -> Int
day1a ints = go ints 0
  where go (x:y:xs) n | y > x = go (y:xs) (n + 1)
        go (x:y:xs) n = go (y:xs) n
        go _ n = n

-- | Solve Day 1 Part Two
-- Consider sums of a three-measurement sliding window.
-- How many sums are larger than the previous sum?
-- >>> day1b day1Example
-- 5
day1b :: [Int] -> Int
day1b ints = day1a $ go ints []
  where go (x:y:z:xs) acc = go (y:z:xs) $ acc <> [x + y + z]
        go _ acc = acc
