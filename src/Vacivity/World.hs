module Vacivity.World where

import Control.Applicative
import Control.Monad.Random
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Common
import Antiqua.Data.Flood

import Vacivity.FOV.ShadowCast
import Vacivity.FOV.Common
import Vacivity.Data.Tile

import Debug.Trace

data Dungeon = Dungeon (A2D.Array2d DTile) Mask

-- True means passable

mkDungeon :: RandomGen g => Rand g (Set.Set XY, A2D.Array2d TileType) -> Rand g (XY, Dungeon)
mkDungeon mkRand = do
    (pts, ttypes) <- mkRand
    let (pt1, pt2) = case Set.minView pts of
                              Just (x, _) -> let spawn = traceShow x $ floodFind ttypes x in
                                             let stairs = floodFind ttypes spawn in
                                             (spawn, stairs)
                              Nothing -> error "empty dungeon"
    let withSpawn = A2D.putv Spawn ttypes pt1
    let withStair = A2D.putv Stair withSpawn pt2
    let tiles = (DTile []) <$> withStair
    let mask = ((==) Free) <$> withStair
    return $ (pt1, Dungeon tiles mask)

--try tracing all four corners of the tile
fov :: XY -> Int -> Mask -> Mask
fov pos radius mask =
    let lit = calcFOV mask pos radius in
    let thefold = foldl (A2D.putv True) mask lit in
    thefold

