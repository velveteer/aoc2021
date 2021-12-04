{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Advent.Day3
-- Description : Day 3 Solutions

-- <https://adventofcode.com/2021/day/3>

module Advent.Day3 where

import           Advent.Prelude
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Numeric        (readInt)

day3 :: IO ()
day3 = do
  putStrLn "day 3"
  input <- load "day3.txt"
  print $ day3a input
  print $ day3b input

day3Example :: [String]
day3Example =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

-- | Solve Day 3 Part One
-- >>> day3a day3Example
-- 198
day3a :: [String] -> Int
day3a strs = fromBin gamma * fromBin epsilon
  where
   (gamma, epsilon) =
     flip foldMap [0..length (head strs) - 1] $ \col ->
       let column = transpose strs !! col in
         ([mostCommonBit column], [leastCommonBit column])

fromBin :: String -> Int
fromBin = fst . head . readInt 2 (const True) digitToInt

mostCommonBit :: String -> Char
mostCommonBit = fst . maximumBy (comparing snd) . Map.toList . histogram

leastCommonBit :: String -> Char
leastCommonBit = fst . minimumBy (comparing snd) . Map.toList . histogram

-- | Solve Day 3 Part Two
-- >>> day3b day3Example
-- 230
day3b :: [String] -> Int
day3b strs = fromBin (o2Rating strs) * fromBin (co2Rating strs)

o2Rating :: [String] -> String
o2Rating strs = go 0 strs
  where
    go !col (x:[]) = x
    go !col xs
      = let mostCommon = mostCommonBit $ transpose xs !! col
         in go (col + 1) (filter (\str -> (str !! col) == mostCommon) xs)

co2Rating :: [String] -> String
co2Rating strs = go 0 strs
  where
    go !col (x:[]) = x
    go !col xs
      = let leastCommon = leastCommonBit $ transpose xs !! col
         in go (col + 1) (filter (\str -> (str !! col) == leastCommon) xs)
