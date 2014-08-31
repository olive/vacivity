module Vacivity.Data.Tile where

import Prelude hiding (any)
import Control.Applicative
import Data.Foldable

import Antiqua.Common
import Antiqua.Data.Graph
import Antiqua.Data.Coordinate
import Antiqua.Data.Array2d


data TileType = Wall | Free | Solid | Stair | Spawn deriving (Eq)

data Item

data DTile = DTile [Item] TileType


isFree :: DTile -> Bool
isFree (DTile _ Free) = True
isFree _ = False


instance Graph (Array2d TileType) XY where
    neighbors arr pt =
        let dirs = (pt |+|) <$> [(0,1), (1,0), (-1,0),(0,-1)] in
        let free p = any ((==) Free) $ get arr p in
        (,1) <$> filter (\p -> free p && inRange arr p) dirs
