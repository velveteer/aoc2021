module Advent.Prelude
  ( module Export
  , (<$$>)
  , load
  , asInts
  , histogram
  ) where

import           Prelude             as Export

import           Control.Applicative as Export
import           Control.Arrow       as Export ((&&&))
import           Control.Monad       as Export
import           Data.Bifunctor      as Export
import           Data.Bitraversable  as Export
import           Data.Char           as Export (digitToInt, isDigit)
import           Data.Either         as Export
import           Data.Function       as Export
import           Data.Functor        as Export ((<&>))
import           Data.List           as Export
import           Data.List.Split     as Export
import           Data.Map.Strict     as Export (Map)
import           Data.Maybe          as Export
import           Data.Ord            as Export
import           Data.Set            as Export (Set)
import           Data.Text           as Export (Text, pack, unpack)
import           Data.Traversable    as Export
import           Text.Read           as Export (readMaybe)

import qualified Data.Map.Strict     as Map

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

infixl 1 <$$>
(<$$>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
f <$$> b = (fmap . fmap) f b

histogram :: String -> Map Char Int
histogram str = Map.fromListWith (+) (zip str $ repeat 1)

asInts :: [String] -> [Int]
asInts = fmap read
