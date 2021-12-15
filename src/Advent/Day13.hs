{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day13
-- Description : Day 13 Solutions

-- <https://adventofcode.com/2021/day/13>

module Advent.Day13
  ( day13
  ) where

import           Advent.Prelude
import           Data.Set (Set)
import qualified Data.Set as Set

day13 :: IO ()
day13 = do
  putStrLn "day 13"
  input <- load "day13.txt"
  print $ day13a input
  day13b input

day13Example :: [String]
day13Example =
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"
  ]

-- | Solve Day 13 Part One
-- >>> day13a day13Example
-- 17
day13a :: [String] -> Int
day13a = length . uncurry solve . first (take 1) . parse

-- | Solve Day 13 Part Two
-- >>> day13b day13Example
-- #####
-- #...#
-- #...#
-- #...#
-- #####
day13b :: [String] -> IO ()
day13b = void . traverse putStrLn . printPaper . uncurry solve . parse

solve :: [(String, Int)] -> Set (Int, Int) -> [(Int, Int)]
solve folds coords = Set.toList $ foldl' go coords folds
  where
    go acc (c, i) = foldMap (go2 c i) acc
    go2 "x" i (x, y) | x > i = Set.singleton (i - abs (x - i), y)
    go2 "y" i (x, y) | y > i = Set.singleton (x, i - abs (y - i))
    go2 _   _ (x, y)         = Set.singleton (x, y)

parse :: [String] -> ([(String, Int)], Set (Int, Int))
parse strs = (folds, coords)
  where
    (foldMap parseFolds -> folds, foldMap parseCoords -> coords)
      = partition (isPrefixOf "fold") strs

parseCoords :: String -> Set (Int, Int)
parseCoords (splitOn "," -> [read -> a, read -> b]) = Set.singleton (a, b)
parseCoords _ = mempty

parseFolds :: String -> [(String, Int)]
parseFolds (words -> [_,_,splitOn "=" -> [c, read -> i]]) = [(c, i)]
parseFolds _ = []

printPaper :: [(Int, Int)] -> [String]
printPaper cs =
  flip concatMap [0..maxY] $ \y ->
    for [0..maxX] $ \x ->
      if (x,y) `elem` cs then "#" else "."
  where
    maxY = maximum $ snd <$> cs
    maxX = maximum $ fst <$> cs
