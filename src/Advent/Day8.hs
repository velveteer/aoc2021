{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Advent.Day8
-- Description : Day 8 Solutions

-- <https://adventofcode.com/2021/day/8>

module Advent.Day8
  ( day8
  ) where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day8 :: IO ()
day8 = do
  putStrLn "day 8"
  input <- load "day8.txt"
  print $ day8a input
  print $ day8b input

day8Example :: [String]
day8Example =
  [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

-- | Solve Day 8 Part One
-- >>> day8a day8Example
-- 26
day8a :: [String] -> Int
day8a strs = length $ flip foldMap (parse strs) $ \(_, d) -> filter isUnique d

-- | Solve Day 8 Part Two
-- >>> day8b day8Example
-- 61229
day8b :: [String] -> Int
day8b strs = sum $ uncurry solve <$> parse strs

solve :: [String] -> [String] -> Int
solve u d = read $ mapMaybe (m Map.!?) d
  where
    m = Map.fromList
      [ (zero, '0')
      , (one, '1')
      , (two, '2')
      , (three, '3')
      , (four, '4')
      , (five, '5')
      , (six, '6')
      , (seven, '7')
      , (eight, '8')
      , (nine, '9')
      ]
    [two]   = filter (\s -> length s == 5 && length (s `intersect` four) == 2) u
    [zero]  = filter (\s -> length s == 6 && length (s \\ five)          == 2) u
    [five]  = filter (\s -> length s == 5 && length (six \\ s)           == 1) u
    [three] = filter (\s -> length s == 5 && length (s `intersect` one)  == 2) u
    [nine]  = filter (\s -> length s == 6 && length (s \\ four)          == 2) u
    [six]   = filter (\s -> length s == 6 && length (s `intersect` one)  == 1) u
    (one:seven:four:eight:[]) = sortBy (comparing length) $ filter isUnique u

isUnique :: String -> Bool
isUnique str = length str `elem` [2, 3, 4, 7]

parse :: [String] -> [([String], [String])]
parse = fmap (join bimap (fmap sort . words) . pair)
  where
    pair s = let [u, d] = splitOn "|" s in (u, d)
