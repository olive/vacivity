module Vacivity.FOV.ShadowCast(
    calcFOV
) where

import Prelude hiding (any, all, foldl)
import Data.Foldable
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Common

import Vacivity.FOV.Common
import Vacivity.Utils

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
                               lit :: Set.Set XY
                             }

calcFOV :: Mask -> XY -> Int -> Set.Set XY
calcFOV msk@(A2D.Array2d cols rows _) (sx, sy) r =
    let size = (cols, rows) in
    let dirs = [ (i, j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0 ] in
    let cast = castLight size 1 1.0 0.0 msk in
    let seed = Set.singleton (sx, sy) in
    foldl (\l (i, j) -> (cast i 0 0 j . cast 0 i j 0) l) seed dirs
 where castLight size@(w,h) row start end mask xx xy yx yy l =
           if start < end
           then l
           else lit $ outer row (ShadowArgs start 0.0 False l)
        where recast d st ed lit' = castLight size d st ed mask xx xy yx yy lit'
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
                       else let lit' = onlyIf (inRadius (dx, dy) r) (Set.insert pos) (lit args) in
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


