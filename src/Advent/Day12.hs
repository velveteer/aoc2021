-- |
-- Module      : Advent.Day12
-- Description : Day 12 Solutions

-- <https://adventofcode.com/2021/day/12>

module Advent.Day12
  ( day12
  ) where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day12 :: IO ()
day12 = do
  putStrLn "day 12"
  input <- load "day12.txt"
  print $ day12a input
  print $ day12b input

day12Example :: [String]
day12Example =
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

-- | Solve Day 12 Part One
-- >>> day12a day12Example
-- 10
day12a :: [String] -> Int
day12a = solve (\path x -> not $ all isLower x && elem x path)

-- | Solve Day 12 Part Two
-- >>> day12b day12Example
-- 36
day12b :: [String] -> Int
day12b = solve (\path x -> not $ smallCaveTwice path && all isLower x && elem x path)

solve :: ([String] -> String -> Bool) -> [String] -> Int
solve pr strs = length $ concatMap (go []) (g Map.! "start")
  where
    g = Map.fromListWith (<>) vs
    vs = concatMap connected strs
    go p x | x == "end" = [p]
    go p x = let path = x:p in concatMap (go path) $ filter (pr path) (g Map.! x)

connected :: String -> [(String, [String])]
connected str
  = filter (not . any (== "start") . snd)
  $ [(a, [b]), (b, [a])]
  where [a, b] = splitOn "-" str

smallCaveTwice :: [String] -> Bool
smallCaveTwice path = length smallPath /= length (nub smallPath)
 where
  smallPath = filter (all isLower) path
