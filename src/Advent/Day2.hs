-- |
-- Module      : Advent.Day2
-- Description : Day 2 Solutions

-- <https://adventofcode.com/2021/day/2>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Advent.Day2 where

import           Advent.Prelude

day2 :: IO ()
day2 = do
  putStrLn "day 2"
  input <- load "day2.txt"
  print $ day2a input
  print $ day2b input

day2Example :: [String]
day2Example =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]

-- | Solve Day 2 Part One
-- forward X increases the horizontal position by X units.
-- down X increases the depth by X units.
-- up X decreases the depth by X units.
-- Calculate the horizontal position and depth you would have after
-- following the planned course. What do you get if you multiply your final
-- horizontal position by your final depth?
-- >>> day2a day2Example
-- 150
day2a :: [String] -> Int
day2a strs = let (h, d) = foldl' go (0, 0) strs in h * d
  where
    go (!h, !d) (words -> [cmd, read -> n]) =
      case cmd of
        "forward" -> (h + n, d)
        "up"      -> (h, d - n)
        "down"    -> (h, d + n)

-- | Solve Day 2 Part Two
-- down X increases your aim by X units.
-- up X decreases your aim by X units.
-- forward X does two things:
--   It increases your horizontal position by X units.
--   It increases your depth by your aim multiplied by X.
-- What do you get if you multiply your final
-- horizontal position by your final depth?
-- >>> day2b day2Example
-- 900
day2b :: [String] -> Int
day2b strs = let (h, d, _) = foldl' go (0, 0, 0) strs in h * d
  where
    go (!h, !d, !a) (words -> [cmd, read -> n]) =
      case cmd of
        "forward" -> (h + n, d + (a * n), a)
        "up"      -> (h, d, a - n)
        "down"    -> (h, d, a + n)
