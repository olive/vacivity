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
    foldl (\lit (i, j) -> let l1 = cast 0 i j 0 lit in
                          let l2 = cast i 0 0 j l1 in
                          l2
          ) (Set.singleton (sx, sy)) dirs

 where castLight size@(w,h) row start end mask xx xy yx yy l =
           if start < end
           then l
           else outer row start 0.0 False l
        where recast d st ed lit = castLight size d st ed mask xx xy yx yy lit
              outer d s ns blck lit =
                  if d > r || blck
                  then lit
                  else let (s', ns', blck', lit') = inner d (-d) (-d) s ns blck lit in
                       outer (d + 1) s' ns' blck' lit'
              inner d dy dx s ns blck lit =
                  let reinner = inner d dy (dx + 1) in
                  if dx > 0
                  then (s, ns, blck, lit)
                  else let pos = (sx + dx * xx + dy * xy
                                 ,sy + dx * yx + dy * yy) in
                       let f :: Int -> Int -> Double -> Double -> Double
                           f x y sigx sigy =   (fromIntegral x + sigx*0.5)
                                             / (fromIntegral y + sigy*0.5)
                       in
                       let ls = f dx dy (-1) 1 in
                       let rs = f dx dy 1 (-1) in
                       if not (inRange pos (0, 0, w, h)) || s < rs
                       then reinner s ns blck lit
                       else if end > ls
                       then (s, ns, blck, lit)
                       else let lit' = onlyIf (inRadius (dx, dy) r) (Set.insert pos) lit in
                            let solid = isSolid mask pos in
                            if blck
                            then if solid
                                 then reinner s rs blck lit'
                                 else reinner ns ns False lit'
                            else if solid && d < r
                                 then let lit'' = recast (d + 1) s ls lit' in
                                      reinner s rs True lit''
                            else reinner s ns blck lit'


