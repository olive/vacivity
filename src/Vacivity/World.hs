module Vacivity.World where

import Control.Applicative
import Control.Monad.Random
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Common

import Vacivity.FOV.ShadowCast
import Vacivity.FOV.Common
import Vacivity.Data.Tile




data Dungeon = Dungeon (A2D.Array2d DTile) Mask

-- True means passable

mkDungeon :: RandomGen g => Rand g (A2D.Array2d TileType) -> Rand g Dungeon
mkDungeon mkRand = do
    ttypes <- mkRand
    let tiles = (DTile []) <$> ttypes
    let mask = ((==) Free) <$> ttypes
    return $ Dungeon tiles mask

--try tracing all four corners of the tile
fov :: XY -> Int -> Mask -> Mask
fov pos radius mask =
    let lit = Set.toList $ calcFOV mask pos radius in
    let dark = const False <$> mask in
    foldl (A2D.putv True) dark lit
