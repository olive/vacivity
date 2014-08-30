module Vacivity.World where

import Control.Applicative
import Control.Monad.Random
import Data.Maybe
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import qualified Antiqua.Geometry.Circle as Circle
import qualified Antiqua.Geometry.Line as Line
import Antiqua.Common

import Vacivity.FOV.ShadowCast
import Vacivity.Data.Tile

import Debug.Trace



data Dungeon = Dungeon (A2D.Array2d DTile) Mask

-- True means passable

mkDungeon :: RandomGen g => Rand g (A2D.Array2d TileType) -> Rand g Dungeon
mkDungeon mkRand = do
    ttypes <- mkRand
    let tiles = (DTile []) <$> ttypes
    let mask = ((==) Free) <$> ttypes
    return $ Dungeon tiles mask

--try tracing all four corners of the tile
trace :: XY -> Int -> Mask -> Mask
trace pos radius mask =
    let lit = Set.toList $ calculate mask pos radius in
    let dark = const False <$> mask in
    foldl (A2D.putv True) dark lit
    --let circle = Circle.bresenham pos radius in
    --let lit = filter (not . blocked mask pos) circle in
    --let dark = const False <$> mask in
    --foldl (A2D.putv True) dark lit
    --where blocked :: Mask -> XY -> XY -> Bool
    --      blocked msk center pt =
    --          any not $ catMaybes $ (A2D.get msk) <$> (einit . etail $ Line.bresenham pt center)
    --      etail [] = []
    --      etail (_:xs) = xs
    --      einit [] = []
    --      einit ls@(_:_) = init ls
