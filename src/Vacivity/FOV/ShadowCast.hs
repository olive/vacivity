module Vacivity.FOV.ShadowCast(
    calcFOV
) where

import Prelude hiding (any, all, foldl, concat)
import Control.Applicative ((<$>))
import Data.Foldable hiding (toList)
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Common

import Vacivity.FOV.Common
import Vacivity.Utils

import Debug.Trace

inRadius :: XY -> Int -> Bool
inRadius (x, y) r =
    x*x + y*y <= r*r

inRange :: XY -> (Int,Int,Int,Int) -> Bool
inRange (x, y) (rx, ry, rw, rh)
    | x >= rx && y >= ry && x < rx + rw && y < ry + rh = True
    | otherwise = False

isSolid :: Mask -> XY -> Bool
isSolid msk c = any not (A2D.get msk c)

data ShadowArgs = ShadowArgs { s :: Double,
                               ns :: Double,
                               b :: Bool,
                               lit :: Col XY
                             }

type Col a = Set.Set a

toList :: Col a -> [a]
toList = Set.toList

append :: Ord a => a -> Col a -> Col a
append x = Set.insert x

single :: a -> Col a
single = Set.singleton

empty :: Col a
empty = Set.empty

calcFOV :: Mask -> XY -> Int -> [XY]
calcFOV msk@(A2D.Array2d w h _) (sx, sy) r =
    let dirs = [ (-1,1), (1,-1), (-1,-1), (1,1) ] in
    let cast = castLight 1 1.0 0.0 msk in
    let seed = single (sx, sy) in
    let calls = concat $ (\(i, j) -> [cast i 0 0 j, cast 0 i j 0]) <$> dirs in
    let lsts = ($ empty) <$> calls in
    toList $ Set.unions (seed:lsts)
 where castLight row start end mask xx xy yx yy l =
           if start < end
           then l
           else lit $ outer row (ShadowArgs start 0.0 False l)
        where recast d st ed lit' = castLight d st ed mask xx xy yx yy lit'
              outer d args =
                  if d > r || b args
                  then args
                  else (outer (d + 1) . inner d (-d) (-d)) args
              inner d dy dx args =
                  let reinner = inner d dy (dx + 1) in
                  if dx > 0
                  then args
                  else let pos = (sx + dx * xx + dy * xy
                                 ,sy + dx * yx + dy * yy) in
                       let f sigx sigy =   (fromIntegral dx + sigx*0.5)
                                         / (fromIntegral dy + sigy*0.5)
                       in
                       let ls = f (-1) 1 in
                       let rs = f 1 (-1) in
                       if (not . inRange pos) (0, 0, w, h) || s args < rs
                       then reinner args
                       else if end > ls
                       then args
                       else let lit' = onlyIf (inRadius (dx, dy) r) (append pos) (lit args) in
                            let solid = isSolid mask pos in
                            let args' = args { lit = lit' } in
                            reinner $ if b args'
                                      then if solid
                                           then args' { ns = rs }
                                           else args' { s = ns args', b = False}
                                      else if solid && d < r
                                           then let lit'' = recast (d + 1) (s args') ls lit' in
                                                args' { ns = rs, b = True, lit = lit'' }
                                      else args'


