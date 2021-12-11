{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Advent.Day10
-- Description : Day 10 Solutions

-- <https://adventofcode.com/2021/day/10>

module Advent.Day10
  ( day10
  ) where

import           Advent.Prelude
import qualified Data.Map.Strict as Map

day10 :: IO ()
day10 = do
  putStrLn "day 10"
  input <- load "day10.txt"
  print $ day10a input
  print $ day10b input

day10Example :: [String]
day10Example =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

-- | Solve Day 10 Part One
-- >>> day10a day10Example
-- 26397
day10a :: [String] -> Int
day10a strs = sum $ mapMaybe (flip lookup scores <=< go []) strs
  where
    go s (x:xs) | x `elem` openers = go (x:s) xs
    go (s:ss) (x:xs) | x `elem` closers && closes s x = go ss xs
    go _ (x:xs) | x `elem` closers = Just x
    go _ [] = Nothing
    scores = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

-- | Solve Day 10 Part Two
-- >>> day10b day10Example
-- 288957
day10b :: [String] -> Int
day10b strs = sort comps !! (length comps `div` 2)
  where
    go s (x:xs) | x `elem` openers = go ((closer x):s) xs
    go (s:ss) (x:xs) | x == s = go ss xs
    go s [] = Just s
    go _ _ = Nothing
    comps = mapMaybe (fmap score . go []) strs
    scores = [(')', 1), (']', 2), ('}', 3), ('>', 4)]
    score = foldl' calc 0
    calc acc c = (acc * 5) + (fromMaybe 0 $ lookup c scores)

closer :: Char -> Char
closer = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

closes :: Char -> Char -> Bool
closes a b = b == closer a

openers :: String
openers = "[({<"

closers :: String
closers = "])}>"
