{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day9
-- Description : Day 9 Solutions

-- <https://adventofcode.com/2021/day/9>

module Advent.Day9
  ( day9
  ) where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day9 :: IO ()
day9 = do
  putStrLn "day 9"
  input <- load "day9.txt"
  print $ day9a input
  print $ day9b input

day9Example :: [String]
day9Example =
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

type Coord = (Int, Int)
type Grid = Map Coord Char

-- | Solve Day 9 Part One
-- >>> day9a day9Example
-- 15
day9a :: [String] -> Int
day9a strs
  = sum
  . fmap (succ . digitToInt . fst)
  . filter lowPoint
  $ Map.elems adjs
  where
    lowPoint (c, fmap fst -> as) = all (\ch -> ch > c) as
    adjs = Map.mapWithKey (getAdjacents grid) grid
    grid = buildGrid strs

getAdjacents :: Grid -> Coord -> Char -> (Char, [(Char, Coord)])
getAdjacents g coord c =
  (c, mapMaybe (fmap extract $ (g Map.!?) &&& (Just . id)) $ getAdjacent coord)

extract :: (Maybe a, Maybe b) -> Maybe (a, b)
extract (Just a, Just b) = Just (a, b)
extract _                = Nothing

getAdjacent :: Coord -> [Coord]
getAdjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

buildGrid :: [String] -> Grid
buildGrid strs@(row:_) = Map.fromList $ do
  y <- [0..length strs - 1]
  x <- [0..length row - 1]
  [((x, y), strs !! y !! x)]

-- | Solve Day 9 Part Two
-- >>> day9b day9Example
-- 1134
day9b :: [String] -> Int
day9b strs
  = product
  . take 3
  . sortBy (comparing Down)
  . fmap (length . getBasin grid)
  . filter lowPoint
  $ Map.elems adjs
  where
    lowPoint (c, fmap fst -> as) = all (\ch -> ch > c) as
    adjs = Map.mapWithKey (getAdjacents grid) grid
    grid = buildGrid strs

getBasin :: Grid -> (Char, [(Char, Coord)]) -> String
getBasin grid (c, adjs) = fst $ go [c] [] c adjs
  where
    go acc seen l ((c', coord):xs) | c' > l && c' /= '9' && coord `notElem` seen
      = let (a, s) = go (c':acc) (coord:seen) c' (snd $ getAdjacents grid coord c')
            (a', s') = go [] s l xs
         in (a <> a', s')
    go acc seen l ((c', coord):xs) = go acc seen l xs
    go acc seen l [] = (acc, seen)
