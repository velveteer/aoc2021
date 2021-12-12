-- |
-- Module      : Advent.Day11
-- Description : Day 11 Solutions

-- <https://adventofcode.com/2021/day/11>

module Advent.Day11
  ( day11
  ) where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day11 :: IO ()
day11 = do
  putStrLn "day 11"
  input <- load "day11.txt"
  print $ day11a input
  print $ day11b input

day11Example :: [String]
day11Example =
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]

-- | Solve Day 11 Part One
-- >>> day11a day11Example
-- 1656
day11a :: [String] -> Int
day11a = sum . fmap snd . take 101 . octopi

-- | Solve Day 11 Part Two
-- >>> day11b day11Example
-- 195
day11b :: [String] -> Int
day11b strs = let Just n = findIndex (all (== 0) . fst) $ octopi strs in n

type Grid = Map (Int, Int) Int

octopi :: [String] -> [(Grid, Int)]
octopi strs = iterate (uncurry go) (grid, 0)
  where
    grid    = buildGrid strs
    flashed = Map.filter (> 9)
    step    = fmap (+ 1)
    reset   = foldr (flip Map.insert 0)
    go g f
      = let g' = step g
            octs = Map.keys $ flashed g'
         in go2 g' (getNeighbors `concatMap` octs) octs
    go2 g (oct:octs) seen
      = go2 (Map.adjust (+ 1) oct g) octs seen
    go2 g [] seen
      = let newF = Map.keys (flashed g) \\ seen
         in if null newF
            then (reset g seen, length seen)
            else go2 g (getNeighbors `concatMap` newF) (seen <> newF)

buildGrid :: [String] -> Grid
buildGrid strs = Map.fromList $ do
  y <- [0..length strs - 1]
  x <- [0..length (transpose strs) - 1]
  [((x, y), digitToInt $ strs !! y !! x)]

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) =
  [ (x - 1, y - 1)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y + 1)
  , (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]
