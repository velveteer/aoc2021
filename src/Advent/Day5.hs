{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day5
-- Description : Day 5 Solutions

-- <https://adventofcode.com/2021/day/5>

module Advent.Day5 where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day5 :: IO ()
day5 = do
  putStrLn "day 5"
  input <- load "day5.txt"
  print $ day5a input
  print $ day5b input

day5Example :: [String]
day5Example =
  [ "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"
  ]

-- (x1, y1, x2, y2)
type Line = (Int, Int, Int, Int)

-- | Solve Day 5 Part One
-- >>> day5a day5Example
-- 5
day5a :: [String] -> Int
day5a strs = Map.size . Map.filter (> 1) $ grid points
  where
    lines
      = fmap parseLine strs
    points
      = concatMap (liftA2 (<>) horizontal vertical)
      $ lines

-- | Solve Day 5 Part Two
-- >>> day5b day5Example
-- 12
day5b :: [String] -> Int
day5b strs = Map.size . Map.filter (> 1) $ grid points
  where
    lines
      = fmap parseLine strs
    points
      = concatMap (((<>) . liftA2 (<>) horizontal vertical) <*> diagonal)
      $ lines

grid :: [(Int, Int)] -> Map (Int, Int) Int
grid points = Map.fromListWith (+) (zip points $ repeat 1)

horizontal :: Line -> [(Int, Int)]
horizontal (x1, y1, x2, y2) | y1 == y2 && x1 < x2 = zip [x1..x2] $ repeat y1
horizontal (x1, y1, x2, y2) | y1 == y2 && x1 > x2 = zip [x1,x1-1..x2] $ repeat y1
horizontal _ = []

vertical :: Line -> [(Int, Int)]
vertical (x1, y1, x2, y2) | x1 == x2 && y1 < y2 = zip (repeat x1) [y1..y2]
vertical (x1, y1, x2, y2) | x1 == x2 && y1 > y2 = zip (repeat x1) [y1,y1-1..y2]
vertical _ = []

diagonal :: Line -> [(Int, Int)]
diagonal (x1, y1, x2, y2) | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
diagonal (x1, y1, x2, y2) | x1 < x2 && y1 > y2 = zip [x1..x2] [y1,y1-1..y2]
diagonal (x1, y1, x2, y2) | x1 > x2 && y1 < y2 = zip [x1,x1-1..x2] [y1..y2]
diagonal (x1, y1, x2, y2) | x1 > x2 && y1 > y2 = zip [x1,x1-1..x2] [y1,y1-1..y2]
diagonal _ = []

parseLine :: String -> Line
parseLine str = (startX, startY, endX, endY)
  where
    [read -> endX, read -> endY]     = splitOn "," end
    [read -> startX, read -> startY] = splitOn "," start
    [start, _, end]                  = words str
