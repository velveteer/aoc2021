{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day6
-- Description : Day 6 Solutions

-- <https://adventofcode.com/2021/day/6>

module Advent.Day6 where

import           Advent.Prelude
import qualified Data.IntMap.Strict as Map

import Debug.Trace

day6 :: IO ()
day6 = do
  putStrLn "day 6"
  l:[] <- load "day6.txt"
  let input = asInts . splitOn "," $ l
  print $ day6a input
  print $ day6b input

day6Example :: [Int]
day6Example = [3, 4, 3, 1, 2]

-- | Solve Day 6 Part One
-- This is the naive approach, it's quite slow.
-- >>> day6a day6Example
-- 5934
day6a :: [Int] -> Int
day6a ints = length $ check 80 ints
  where
    check !n xs | n == 0 = xs
    check !n xs = check (n - 1) $ concatMap update xs
    update i | i == 0 = [6, 8]
    update i = [pred i]

-- | Solve Day 6 Part Two
-- >>> day6b day6Example
-- 26984457539
day6b :: [Int] -> Int
day6b strs =
  sum $ (iterate step (Map.fromListWith (+) (zip strs $ repeat 1)) !! 256)
  where
    step = Map.fromListWith (+) . concatMap (uncurry update) . Map.toList
    update k v | k == 0 = [(6, v), (8, v)]
    update k v = [(k - 1, v)]
