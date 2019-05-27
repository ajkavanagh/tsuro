module Tile
  ( Tile
  ) where

import           Control.Lens    (both, over)
import           Data.List       (intercalate, nub, sort, sortBy, tails)
import           Data.List.Split (chunksOf)
import           Flow

import           Tile.Internal   (rotateTile, rotatedTilesSet, uniqueTiles)

newtype Tile a = Tile
  { points :: a
  }
