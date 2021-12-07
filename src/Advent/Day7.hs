{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day7
-- Description : Day 7 Solutions

-- <https://adventofcode.com/2021/day/7>

module Advent.Day7 where

import           Advent.Prelude

day7 :: IO ()
day7 = do
  putStrLn "day 7"
  l:[] <- load "day7.txt"
  let input = asInts . splitOn "," $ l
  print $ day7a input
  print $ day7b input

day7Example :: [Int]
day7Example = [16,1,2,0,4,2,7,1,2,14]

-- | Solve Day 7 Part One
-- >>> day7a day7Example
-- 37
day7a :: [Int] -> Int
day7a ints = minimum $ fmap sum $ do
  x <- nub ints
  [fmap (\i -> abs (i - x)) ints]

-- | Solve Day 7 Part Two
-- >>> day7b day7Example
-- 168
day7b :: [Int] -> Int
day7b ints = minimum $ fmap sum $ do
  x <- [minimum ints..maximum ints]
  [fmap (\i -> let n = abs (i - x) + 1 in (n * (n - 1)) `div` 2) ints]
